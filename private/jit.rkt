#lang racket
(require ffi/unsafe)

(require "llvm/ffi/all.rkt"
         "llvm/adjunct.rkt"
         "llvm/pass-table.rkt")

(require "env.rkt"
         "init.rkt"
         "types.rkt"
         "ast.rkt"
         "mod-env-info.rkt"
         "rator.rkt"
         "dump.rkt"
         "utils.rkt")

(provide compile-module
         jit-get-function
         jit-get-function-ptr
         jit-get-racket-type
         jit-verify-module)

(define (mcjit-function-ptr f-sym mod-env)
  (define mcjit-engine (env-get-mcjit mod-env))
  (unless mcjit-engine
    (error "jit not initialized, but calling compile-function"))
  (cast (LLVMGetFunctionAddress mcjit-engine (symbol->string f-sym)) _uint64 _pointer))

(define (jit-get-function f-sym mod)
  (define fptr (jit-get-function-ptr f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
  (cast fptr _pointer f-type))

(define (orc-function-ptr f-sym mod)
  (define-values (error-code target-address)
    (LLVMOrcGetSymbolAddress (env-get-orc mod) (symbol->string f-sym)))
  (printf "error code: ~a\n" error-code)
  (cast target-address _uint64 _pointer))

(define (jit-get-function-ptr f-sym mod)
  (when (not (env-contains? f-sym mod))
    (error "function not in module " f-sym))
  (if (env-get-orc mod)
      (orc-function-ptr f-sym mod)
      (mcjit-function-ptr f-sym mod)))

(define (jit-get-racket-type t-sym mod)
  (internal-type-racket (env-type-prim (env-lookup t-sym mod))))

(define (jit-verify-module mod-env)
  (define jit-mod (env-get-module mod-env))
  (LLVMVerifyModule jit-mod 'LLVMPrintMessageAction #f))

(define (compile-module m [module-name "module"] [context global-jit-context])
  ;; (when (sham-diagnose)
  ;;   (define (diag-handler dinfo voidp)
  ;;     (define diag-desc (LLVMGetDiagInfoDescription dinfo))
  ;;     (printf "\t llvm-diag: ~a\n" (cast diag-desc _pointer _string))
  ;;     (LLVMDisposeMessage diag-desc))
  ;;   (LLVMContextSetDiagnosticHandler context diag-handler #f))

  (define jit-module (LLVMModuleCreateWithNameInContext module-name context))

  (LLVMSetDataLayout
   jit-module
   (string-join
    '("e" "m:e" "p:64:64:64"
          "i1:8:8" "i8:8:8" "i16:16:16" "i32:32:32" "i64:64:64" ;;integer types alignment
          "f32:32:32" "f64:64:64" "f128:128:128" ;;floating types alignment
          "v64:64:64" "v128:128:128" ;; vector type alignment
          "a:0:64" "s0:64:64" "n8:16:32:64" ;; a aggregate alignment
          "S128" ;stack alignment
      )
    "-"))
  (LLVMSetTarget jit-module "x86_64-unknown-linux-gnu")
  (define builder (LLVMCreateBuilderInContext context))
  (define function-info-map (make-hash)) (define add-function-info! (curry hash-set! function-info-map))
  (define type-info-map (make-hash)) (define add-type-info! (curry hash-set! type-info-map))
  (define intrinsic-map (make-hash))
  (define ffi-mappings (make-hash))
  (define rkt-mappings (make-hash))
  (define add-rkt-mapping! (curry hash-set! rkt-mappings))
  (define add-ffi-mapping! (curry hash-set! ffi-mappings))

  (define (register-module-define stmt env)
    (define (compile-function-declaration env-function-type function-name)
      (define function-type (internal-type-jit (env-type-prim env-function-type)))
      (LLVMAddFunction jit-module (symbol->string function-name) function-type))

    (match stmt
      [(sham:def:type info type-name t)
       (env-extend type-name (build-env-type t env) env)]
      [(sham:def:function info function-name args types ret-type body)
       (define type (build-env-type (sham:type:function types ret-type) env))
       (define function-obj (compile-function-declaration type function-name))
       (env-extend function-name (env-jit-function function-obj type) env)]
      [(sham:def:global info id t)
       (define type (build-env-type t env))
       (define value
         (LLVMAddGlobal jit-module
                        (internal-type-jit (env-type-prim type))
                        (symbol->string id)))
       (LLVMSetInitializer value
                           (LLVMConstNull (internal-type-jit (env-type-prim type))))
       (env-extend id (env-jit-value value (build-env-type t env)) env)]))

  (define (compile-module-define def env module-env)
    (define (compile-function-definition name args types ret-type body)
      (define env-function (env-lookup name env))
      (define fref (env-jit-function-ref env-function))
      (define ftype (env-jit-function-type env-function))
      (define jit-target-data (LLVMCreateTargetData (LLVMGetDataLayout jit-module)))
      (define (build-llvm-type t env)
        (internal-type-jit (compile-type t env)))
      (define (new-block n)
        (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                       fref (symbol->string n)))
      (define (build-statement stmt env)
        (match stmt
          [(sham:stmt:set! md lhs v) ;;TODO: right now lhs can only be a var
           (match lhs
             [(sham:expr:var md var)
              (define lhs-v (env-jit-value-ref (env-lookup var env)))
              (define rhs-v (build-expression v env))
              (LLVMBuildStore builder rhs-v lhs-v)]
             [(sham:expr:global md var)
              (define global-v (env-jit-value-ref (env-lookup var env)))
              (define rhs-v (build-expression v env))
              (LLVMSetInitializer global-v rhs-v)])]

          [(sham:stmt:if md tst thn els)
           (define tst-value (build-expression tst env))
           (define then-block (new-block 'then))
           (define else-block (new-block 'else))
           (define end-block (new-block 'ife))

           (LLVMBuildCondBr builder tst-value then-block else-block)

           (LLVMPositionBuilderAtEnd builder then-block)
           (build-statement thn env)
           (define (current-return?)
             (define bbt (LLVMGetBasicBlockTerminator
                          (LLVMGetInsertBlock builder)))
             (if bbt (zero? (LLVMGetNumSuccessors bbt)) #f))

           (define then-return? (current-return?))
           (unless then-return?
             (LLVMBuildBr builder end-block))

           (LLVMPositionBuilderAtEnd builder else-block)
           (build-statement els env)
           (define else-return? (current-return?))
           (unless else-return?
             (LLVMBuildBr builder end-block))

           (if (and then-return? else-return?)
               (LLVMDeleteBasicBlock end-block)
               (LLVMPositionBuilderAtEnd builder end-block))]

          [(sham:stmt:while md tst body)
           (define prev-block (LLVMGetInsertBlock builder))
           (define loop-entry (new-block 'loop-entry))
           (define loop-block (new-block 'loop-block))
           (define afterloop-block (new-block 'afterloop-block))

           (LLVMBuildBr builder loop-entry)

           (LLVMPositionBuilderAtEnd builder loop-entry)
           (define tst-value (build-expression tst env))
           (LLVMBuildCondBr builder tst-value loop-block afterloop-block)

           (LLVMPositionBuilderAtEnd builder loop-block)

           (build-statement body env)
           (define body-end-blk (LLVMGetInsertBlock builder))
           (LLVMBuildBr builder loop-entry)

           (LLVMPositionBuilderAtEnd builder afterloop-block)]

          [(sham:stmt:return md v)
           (if (sham:expr:void? v)
               (LLVMBuildRetVoid builder)
               (LLVMBuildRet builder (build-expression v env)))]

          [(sham:stmt:block md stmts)
           (for ([stmt stmts])
             (build-statement stmt env))]
          [(sham:stmt:void md) (void)]
          [(sham:stmt:expr md e) (build-expression e env)]
          [else (error "unknown statement" stmt)]))

      (define (build-expression e env)
        (match e
          [(sham:expr:let md ids types vals st ex)
           (define new-env
             (for/fold ([env env])
                       ([id-type types]
                        [id ids]
                        [val vals])
               (define val-type (build-llvm-type id-type env))
               ;;TIPS: add alloca at the top of function for performance
               ;; and set lifetime info using metadata
               (define val-p (LLVMBuildAlloca builder val-type (symbol->string id)))
               (unless (sham:expr:void? val) (LLVMBuildStore builder (build-expression val env) val-p))
               (env-extend id (env-jit-value val-p (env-type val-type id-type)) env)))
           (build-statement st new-env)
           (build-expression ex new-env)]
          [(sham:expr:app md rator rands)
           (define rand-values (map (curryr build-expression env) rands))
           (build-app rator rand-values env)]
          [(sham:expr:fl-value md value t) (LLVMConstReal (build-llvm-type t env) value)]
          [(sham:expr:si-value md value t)
           (LLVMConstInt (build-llvm-type t env) (cast value _sint64 _uint64) #f)]
          [(sham:expr:ui-value md value t) (LLVMConstInt (build-llvm-type t env) value #f)]
          [(sham:expr:struct-value md vals)
           (LLVMConstStruct (map (curryr build-expression env) vals))]
          [(sham:expr:array-value md vals t)
           (LLVMConstArray (build-llvm-type t env) (map (curryr build-expression env) vals))]
          [(sham:expr:vector-value md vals t)
           (LLVMConstVector (build-llvm-type t env) (map (curryr build-expression env) vals))]
          [(sham:expr:sizeof md type)
           (define llvm-type (build-llvm-type type env))
           (LLVMConstInt (internal-type-jit (env-type-prim (env-lookup 'i32 env)))
                         (LLVMStoreSizeOfType jit-target-data
                                              llvm-type)
                         #f)]
          [(sham:expr:type md t) (build-llvm-type t env)]
          [(sham:expr:void md) (void)]
          [(sham:expr:gep md ptr indxs)
           (LLVMBuildGEP builder
                         (build-expression ptr env)
                         (map (curryr build-expression env) indxs)
                         "gep")]
          [(sham:expr:var md sym)
           (LLVMBuildLoad builder
                          (env-jit-value-ref (env-lookup sym env))
                          (symbol->string sym))]
          [(sham:expr:external md lib-id sym t)
           (define value
             (LLVMAddGlobal jit-module (build-llvm-type t env) (symbol->string sym)))
           (add-ffi-mapping! sym (cons lib-id value))
           (LLVMBuildLoad builder value (symbol->string sym))]
          [(sham:expr:global md sym) (env-jit-value-ref (env-lookup sym env))]
          [else (error "no matching clause for build-expression" e)]))

      (define (build-app rator rand-values env)
        (match rator
          [(sham:rator:intrinsic str-id ret-type)
           (define s (symbol->string str-id))
           (define ref
             (if (hash-has-key? intrinsic-map s)
                 (hash-ref intrinsic-map s)
                 (let* ([fn-type (LLVMFunctionType (build-llvm-type ret-type env)
                                                   (map LLVMTypeOf rand-values) #f)]
                        [ref (LLVMAddFunction jit-module s fn-type)])
                   (hash-set! intrinsic-map s ref)
                   ref)))
           (LLVMBuildCall builder ref rand-values (substring s 0 3))]
          [(sham:rator:external lib-id id ret-type)
           #:when (hash-has-key? ffi-mappings id)
           (LLVMBuildCall builder (cdr (hash-ref ffi-mappings id)) rand-values "e")]
          [(sham:rator:external lib-id id ret-type)
           (define s (symbol->string id))
           (define fn-type (LLVMFunctionType (build-llvm-type ret-type env)
                                             (map LLVMTypeOf rand-values) #f))
           (define fn-value (LLVMAddFunction jit-module s fn-type))
           (add-ffi-mapping! id (cons lib-id fn-value))
           (LLVMBuildCall builder fn-value rand-values (substring s 0 3))]
          [(sham:rator:racket id rkt-fun type)
           (define s (symbol->string id))
           (define ct (compile-type type env))
           (define fn-type (internal-type-jit ct))
           (define fn-value (LLVMAddFunction jit-module s fn-type))
           (add-rkt-mapping! s (list rkt-fun (internal-type-racket ct) fn-value))
           (LLVMBuildCall builder fn-value rand-values (substring s 0 3))]
          [(sham:rator:symbol sym)
           (match (env-lookup sym env)
             [(env-jit-function ref type)
              (define call-name
                (if (equal? (LLVMGetReturnType (internal-type-jit (env-type-prim type)))
                            (LLVMVoidType))
                    ""
                    (substring (symbol->string sym) 0 3)))
              (LLVMBuildCall builder ref rand-values call-name)]
             [(env-jit-intr-function appbuilder)
              (appbuilder builder rand-values)]
             [else (error "cannot figure out how to apply: ~a" rator)])]))
      (define entry-block (new-block 'entry))
      (LLVMPositionBuilderAtEnd builder entry-block)
      (define new-env
        (for/fold ([env env])
                  ([arg args]
                   [type types]
                   [i (in-range (length args))])
          (define l-type (build-env-type type env))
          (define p-arg (LLVMBuildAlloca builder
                                         (internal-type-jit (env-type-prim l-type))
                                         (symbol->string arg)))
          (LLVMBuildStore builder (LLVMGetParam fref i) p-arg)
          (env-extend arg (env-jit-value p-arg l-type) env)))

      (build-statement body new-env)
      env-function)
    ;; TODO return values of name and obj
    (match def
      [(sham:def:type info type-name t)
       ;types are created in register phase
       ; something for recursive struct types to be done
       ;; TODO for struct switch to creating empty struct type
       ;and then adding fields here, for now we can use opaque pointer and then cast everytime HACK!!
       (add-type-info! type-name info)
       (env-extend type-name (env-lookup type-name env) module-env)]
      [(sham:def:function info function-name args types ret-type body)
       (compile-function-definition function-name args types ret-type body)
       (define f (env-lookup function-name env))
       (define lf (env-jit-function-ref f))
       (add-function-info! function-name info)
       (env-extend function-name f module-env)]
      [(sham:def:global info id t)
       (define env-value (env-lookup id env))
       (env-extend id env-value module-env)]))
  (match m
    [(sham:module info defs)
     (define env
       (for/fold ([env (create-initial-environment context)])
                 ([def defs])
         (register-module-define def env)))
     (define module-env
       (for/fold ([module-env (empty-env)])
                 ([def defs])
         (compile-module-define def env module-env)))
     (env-add-info
      module-env
      (update-info!
       info
       `((,module-key            . ,jit-module)
         (,context-key           . ,context)
         (,per-function-info-key . ,function-info-map)
         (,per-type-info-key     . ,type-info-map)
         (,ffi-mapping-key       . ,ffi-mappings)
         (,rkt-mapping-key       . ,rkt-mappings))))]))


;; (module+ test
;;   (define module-env
;;     (compile-module
;;      (sham:module
;;       (void)
;;       (list
;;        (sham:def:function
;;         (void)
;;         'testf (list 'a) (list (sham:type:ref 'i32)) (sham:type:ref 'void)
;;         (sham:stmt:block
;;          (list
;;           (sham:stmt:expr (sham:expr:app (sham:rator:racket 'test
;;                                                             (λ (a) (printf "got a: ~a" a))
;;                                                             (sham:type:function (list (sham:type:ref 'i32))
;;                                                                                 (sham:type:ref 'void)))
;;                                          (list (sham:expr:var 'a))))
;;           (sham:stmt:return (sham:expr:void)))))))))
;;   (initialize-jit! module-env #:opt-level 3)
;;   (jit-dump-module module-env)
;;   (define t (jit-get-function 'testf module-env)))


;; disabling tests for now
(module+ test
  (require racket/unsafe/ops)
  (require rackunit)
  (require disassemble)
  (require "optimize.rkt")
  (require (submod sham/ast utils))

  (define i32 (sham:type:ref 'i32))
  (define i64 (sham:type:ref 'i64))
  (define icmp-eq (sham:rator:symbol 'icmp-eq))
  (define urem (sham:rator:symbol 'urem))
  (define (build-app rator . rands)
    (sham:expr:app rator rands))
  (define (ui32 v)
    (sham:expr:ui-value v i32))
  (define ret sham:stmt:return)
  (define v sham:expr:var)
  (define rs sham:rator:symbol)
  (define (defn n args types ret-type stmt) (sham:def:function (make-hash) n args types ret-type stmt))
  (define module-env
    (compile-module
     (sham:module
      (empty-module-info)
      (list
       (sham$def:type 'int i32)
       (sham$def:type 'int* (sham:type:pointer i32))
       (sham$def:type 'bc (sham:type:struct '(b c) (list i32 i32)))
       (sham$def:type 'pbc (sham:type:pointer (sham:type:ref 'bc)))
       (sham$def:type 'ui (sham:type:struct '(bc1 bc2) (list (sham:type:ref 'pbc)
                                                             (sham:type:ref 'pbc))))

       (sham:def:global '() 'constant1 i32)
       (sham:def:function (make-hash) 'setglobal '() '() i32
                          (sham:stmt:block
                           (list
                            ;; (sham:stmt:set! (sham:expr:global 'constant1)
                            ;;                 ;(build-app (sham:rator:symbol 'fact) (sham:expr:ui-value 5 i32))
                            ;;                 (sham:expr:ui-value 1 i32)
                            ;;                 )
                            (sham:stmt:set! (sham:expr:var 'constant1)
                                            (build-app (sham:rator:symbol 'fact) (sham:expr:ui-value 5 i32)))
                            (sham:stmt:return (sham:expr:ui-value 1 i32)))))
       (sham:def:function (make-hash) 'const1 '() '() i32
                          (sham:stmt:block
                           (list
                            (sham:stmt:set! (sham:expr:var 'constant1)
                                            (sham:expr:ui-value 2 i32))
                            (sham:stmt:return (sham:expr:var 'constant1)))))
       (sham:def:function (make-hash) 'id '(x) (list i32) i32
                          (ret (v 'x)))

       (defn
         'even? '(x) (list i32) i32
         (sham:stmt:if (build-app icmp-eq (build-app urem (sham:expr:var 'x) (ui32 2))
                                  (ui32 0))
                       (ret (ui32 1))
                       (ret (ui32 0))))

       (defn
         'meven? '(x) (list i32) i32
         (sham:stmt:if (build-app icmp-eq (v 'x) (ui32 0))
                       (ret (ui32 1))
                       (ret (build-app (rs 'modd?)
                                       (build-app (rs 'sub) (v 'x) (ui32 1))))))
       (defn
         'modd?  '(x) (list i32) i32
         (sham:stmt:if (build-app icmp-eq (v 'x) (ui32 0))
                       (ret (ui32 0))
                       (ret (build-app (rs 'meven?)
                                       (build-app (rs 'sub) (v 'x) (ui32 1))))))

       (defn 'fact '(x) (list i32) i32
         (sham:stmt:if (build-app icmp-eq (v 'x) (ui32 0))
                       (ret (ui32 1))
                       (ret (build-app (rs 'mul)
                                       (v 'x)
                                       (build-app (rs 'fact)
                                                  (build-app (rs 'sub)
                                                             (v 'x) (ui32 1)))))))

       (defn 'factr '(x) (list i32) i32
         (sham:stmt:let
          '(result i) (list i32 i32) (list (ui32 1) (ui32 1))
          (sham:stmt:block
           (list
            (sham:stmt:while
             (build-app (rs 'icmp-ule) (v 'i) (v 'x))
             (sham:stmt:block
              (list
               (sham:stmt:set! (v 'result)
                               (build-app (rs 'mul-nuw)
                                          (v 'result) (v 'i)))
               (sham:stmt:set! (v 'i)
                               (build-app (rs 'add-nuw) (v 'i) (ui32 1))))))
            (ret (v 'result))))))

       (defn 'sum-array '(arr size) (list (sham:type:ref 'int*) i32) i32
         (sham:stmt:let
          '(i sum) (list i32 i32) (list (ui32 0) (ui32 0))
          (sham:stmt:block
           (list
            (sham:stmt:while
             (build-app (rs 'icmp-ult) (v 'i) (v 'size))
             (sham:stmt:block
              (list
               (sham:stmt:let '(arri) (list (sham:type:ref 'int*))
                              (list (sham:expr:gep (v 'arr)
                                                   (list (v 'i))))
                              (sham:stmt:set! (v 'sum)
                                              (build-app (rs 'add)
                                                         (v 'sum)
                                                         (build-app (rs 'load) (v 'arri)))))
               (sham:stmt:set! (v 'i) (build-app (rs 'add) (v 'i) (ui32 1))))))
            (ret (v 'sum))))))
       (defn 'if-void-test '() '() i32
         (sham:stmt:block
          (list
           (sham:stmt:if (build-app icmp-eq (ui32 0) (ui32 0))
                         (sham:stmt:expr (sham:expr:void))
                         (sham:stmt:return (ui32 1)))
           (ret (ui32 0)))))
       (defn 'malloc-test '() '() i32
         (sham:stmt:let '(ptr)
                        (list (sham:type:ref 'int*))
                        (list (build-app (rs 'malloc) (sham:expr:type (sham:type:ref 'int))))
                        (sham:stmt:block
                         (list
                          (sham:stmt:expr (build-app (rs 'store!) (ui32 42) (v 'ptr)))
                          (ret (build-app (rs 'load) (v 'ptr)))))))
       ))))
  (optimize-module module-env #:opt-level 3)
  (initialize-orc! module-env)
  ;; (initialize-jit! module-env)
  (define factr (jit-get-function 'factr module-env))
  (disassemble-ffi-function (jit-get-function-ptr 'factr module-env) #:size 200))
