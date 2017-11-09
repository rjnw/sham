#lang racket
(require ffi/unsafe)

(require "llvm/ffi/all.rkt"
         "llvm/adjunct.rkt"
         "llvm/global-mapping.rkt"
         "llvm/pass-table.rkt")

(require "env.rkt"
         "types.rkt"
         "ast.rkt"
         "internals.rkt"
         "utils.rkt")

(provide
 initialize-jit
 jit-dump-module
 jit-dump-function
 jit-write-bitcode
 jit-write-module
 jit-get-racket-type
 jit-get-function
 jit-get-function-ptr
 jit-verify-module
 jit-optimize-module
 jit-optimize-function
 jit-run-module-pass-env
 jit-run-function-pass-env
 compile-module
 build-info
 env-lookup)

(define (llvm-initialize)
  (LLVMLinkInMCJIT)
  (LLVMInitializeX86Target)
  (LLVMInitializeX86TargetInfo)
  (LLVMInitializeX86TargetMC)
  (LLVMInitializeX86AsmParser)
  (LLVMInitializeX86AsmPrinter)
  (define gpr (LLVMGetGlobalPassRegistry))
  (LLVMInitializeCore gpr)
  (LLVMInitializeTransformUtils gpr)
  (LLVMInitializeScalarOpts gpr)
  (LLVMInitializeObjCARCOpts gpr)
  (LLVMInitializeVectorization gpr)
  (LLVMInitializeInstCombine gpr)
  (LLVMInitializeIPO gpr)
  (LLVMInitializeInstrumentation gpr)
  (LLVMInitializeAnalysis gpr)
  (LLVMInitializeIPA gpr)
  (LLVMInitializeCodeGen gpr)
  (LLVMInitializeTarget gpr))
(llvm-initialize)

(define (create-jit-context)
  (LLVMContextCreate))
(define global-jit-context (LLVMGetGlobalContext))

(define (create-initial-environment context)
  (register-jit-internals (register-initial-types (empty-env) context) context))

(define (jit-dump-module mod)
  (LLVMDumpModule (jit-get-module mod))
  (for [(m mod)]
    (let ([s (car m)]
          [v (cdr m)])
      (cond ([env-type? v]
             (printf "type ~a: ~a\n" s
                     (LLVMPrintTypeToString (internal-type-jit (env-type-prim v)))))
            ([env-jit-function? v]
             (LLVMDumpValue (env-jit-function-ref v)))))))

(define (jit-dump-function mod sym)
  (LLVMDumpValue (env-jit-function-ref (env-lookup sym mod))))

(define (build-info cinfo assocs)
  (define new-info (make-hash assocs))
  (when (hash? cinfo)
    (for ([(key value) (in-hash cinfo)])
      (unless (hash-has-key? new-info key)
        (hash-set! new-info key value))))
  new-info)

(define (jit-add-info mod-env info)
  (env-extend '#%jit-info info mod-env))
(define (jit-get-info mod-env)
  (env-lookup '#%jit-info mod-env))

(define (jit-get-info-key sym mod-env)
  (define info  (jit-get-info mod-env))
  (hash-ref info sym (void)))

(define (jit-add-info-key! key val mod-env)
  (define info (jit-get-info mod-env))
  (hash-set! info key val))

(define (jit-get-module mod-env)
  (jit-get-info-key 'jit-module mod-env))

(define (jit-write-bitcode mod fname)
  (LLVMWriteBitcodeToFile (jit-get-module mod) fname))
(define (jit-write-module mod fname)
  (LLVMPrintModuleToFile (jit-get-module mod) fname))

(define (add-ffi-mapping engine mod-env)
  (define ffi-mappings (jit-get-info-key 'ffi-mappings mod-env))
  (unless (empty? ffi-mappings)
    (define ffi-libs (jit-get-info-key 'ffi-libs mod-env))
    (define lib-map (for/hash ([fl ffi-libs])
                      (match fl
                        [`(,lib-name . (,str-args ... #:global? ,gv))
                         (define flib (apply ffi-lib str-args #:global? gv))
                         (values lib-name flib)]
                        [`(,lib-name . (,str-args ...))
                         (define flib (apply ffi-lib str-args))
                         (values lib-name flib)])))
    (for [(ffi-mapping ffi-mappings)]
      (match ffi-mapping
        [`(,name ,lib-name ,value)
         (define fflib (hash-ref lib-map lib-name))
         (LLVMAddGlobalMapping engine value
                               (get-ffi-pointer fflib (symbol->string name)))]))))

(define (initialize-jit mod-env #:opt-level [opt-level 1])
  (define mcjit-options (LLVMInitializeMCJITCompilerOptions))
  (set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options opt-level)
  (define-values (engine status err)
    (LLVMCreateMCJITCompilerForModuleWithTarget (jit-get-module mod-env) mcjit-options))
  (if status
      (error "error initializing jit" status err)
      (begin
        (add-ffi-mapping engine mod-env)
        (jit-add-info-key! 'mcjit-engine engine mod-env)))
  mod-env)
  ;; (initialize-orc-jit mod-env)

(define (initialize-orc-jit mod-env)
  (define orc (LLVMOrcCreateInstance (LLVMCreateCurrentTargetMachineRef)))
  (jit-add-info-key! 'orc-jit orc mod-env))
  ;; (LLVMOrcAddEagerlyCompiledIR orc (jit-get-module mod-env) symbolResolver orc)


;; TODO check for mcjit-engine in env
(define (jit-compile-function f-sym mod)
  (define mcjit-engine (jit-get-info-key 'mcjit-engine mod))
  (cast (LLVMGetFunctionAddress mcjit-engine
                                (symbol->string f-sym))
        _uint64 _pointer))

(define (jit-get-function f-sym mod)
  (define fptr (jit-compile-function f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
  (cast fptr _pointer f-type))

(define (jit-get-function-ptr f-sym mod)
  (define fptr (jit-compile-function f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
  fptr)

(define (jit-get-racket-type t-sym mod)
  (internal-type-racket (env-type-prim (env-lookup t-sym mod))))



(define (jit-run-function-pass passes jit-mod f)
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (for ([pass passes])
    ((jit-lookup-pass pass) fpm))
  (begin0
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref f))
    (LLVMDisposePassManager fpm)))

(define (jit-run-function-pass-env passes f-sym mod-env)
  (define jit-mod (jit-get-module mod-env))
  (jit-run-function-pass passes (env-lookup f-sym mod-env) jit-mod))

(define (jit-run-module-pass passes jit-mod)
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes])
    (define module-pass (jit-lookup-pass pass))
    (module-pass mpm))
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)))

(define (jit-run-module-pass-env passes mod-env)
  (define jit-mod (jit-get-module mod-env))
  (jit-run-module-pass passes jit-mod))

(define (jit-verify-module mod-env)
  (define jit-mod (jit-get-module mod-env))
  (LLVMVerifyModule jit-mod 'LLVMPrintMessageAction #f))

(define (jit-optimize-function mod-env #:opt-level [level 1])
  (define jit-mod (jit-get-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))

(define (jit-optimize-module mod-env #:opt-level [level 1])
  (define jit-mod (jit-get-module mod-env))
  (define mpm (LLVMCreatePassManager))
  (define pmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel pmb level)
  (LLVMPassManagerBuilderSetSizeLevel pmb 100)
  (LLVMPassManagerBuilderPopulateModulePassManager pmb mpm)
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)
    (LLVMPassManagerBuilderDispose pmb)))

(define (jit-run-basic-pass mod-env)
  (define jit-mod (jit-get-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (LLVMAddCFGSimplificationPass fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm))



(define (compile-module m [module-name "module"] [context global-jit-context])
  (define (diag-handler dinfo voidp)
    (void))
  ;; TODO fix memory bugs
  ;; (define diag-desc (LLVMGetDiagInfoDescription dinfo))
  ;; (printf "\tllvm-diag: ~a\n" diag-desc)
  ;; (LLVMDisposeMessage diag-desc)

  (LLVMContextSetDiagnosticHandler context diag-handler #f)
  (define jit-module (LLVMModuleCreateWithNameInContext module-name context))
  (LLVMSetDataLayout jit-module "e-m:e-i64:64-f80:128-n8:16:32:64-S128")
  (LLVMSetTarget jit-module "x86_64-unknown-linux-gnu")
  (define jit-builder (LLVMCreateBuilderInContext context))
  (define ffi-mappings (box '()))
  (define intrinsic-map (make-hash))
  (define (add-mapping! mapping)
    (set-box! ffi-mappings (cons mapping (unbox ffi-mappings))))

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
              (LLVMBuildStore jit-builder rhs-v lhs-v)]
             [(sham:expr:global md var)
              (define global-v (env-jit-value-ref (env-lookup var env)))
              (define rhs-v (build-expression v env))
              (LLVMSetInitializer global-v rhs-v)])]

          [(sham:stmt:if md tst thn els)
           (define tst-value (build-expression tst env))
           (define then-block (new-block 'then))
           (define else-block (new-block 'else))
           (define end-block (new-block 'ife))

           (LLVMBuildCondBr jit-builder tst-value then-block else-block)

           (LLVMPositionBuilderAtEnd jit-builder then-block)
           (build-statement thn env)
           (define (current-return?)
             (define bbt (LLVMGetBasicBlockTerminator
                          (LLVMGetInsertBlock jit-builder)))
             (if bbt (zero? (LLVMGetNumSuccessors bbt)) #f))

           (define then-return? (current-return?))
           (unless then-return?
             (LLVMBuildBr jit-builder end-block))

           (LLVMPositionBuilderAtEnd jit-builder else-block)
           (build-statement els env)
           (define else-return? (current-return?))
           (unless else-return?
             (LLVMBuildBr jit-builder end-block))

           (if (and then-return? else-return?)
               (LLVMDeleteBasicBlock end-block)
               (LLVMPositionBuilderAtEnd jit-builder end-block))]

          [(sham:stmt:while md tst body)
           (define prev-block (LLVMGetInsertBlock jit-builder))
           (define loop-entry (new-block 'loop-entry))
           (define loop-block (new-block 'loop-block))
           (define afterloop-block (new-block 'afterloop-block))

           (LLVMBuildBr jit-builder loop-entry)

           (LLVMPositionBuilderAtEnd jit-builder loop-entry)
           (define tst-value (build-expression tst env))
           (LLVMBuildCondBr jit-builder tst-value loop-block afterloop-block)

           (LLVMPositionBuilderAtEnd jit-builder loop-block)

           (build-statement body env)
           (define body-end-blk (LLVMGetInsertBlock jit-builder))
           (LLVMBuildBr jit-builder loop-entry)

           (LLVMPositionBuilderAtEnd jit-builder afterloop-block)]

          [(sham:stmt:return md v)
           (if (sham:expr:void? v)
               (LLVMBuildRetVoid jit-builder)
               (LLVMBuildRet jit-builder (build-expression v env)))]

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
               (define val-p (LLVMBuildAlloca jit-builder val-type
                                              (symbol->string id)))
               (unless (sham:expr:void? val)
                 (LLVMBuildStore jit-builder (build-expression val env) val-p))
               (env-extend id
                           (env-jit-value val-p (env-type val-type id-type)) env)))
           (build-statement st new-env)
           (build-expression ex new-env)]
          [(sham:expr:app md rator rands)
           (define rand-values (for/list ([rand rands]) (build-expression rand env)))
           (build-app rator rand-values env)]
          [(sham:expr:fl-value md value t)
           (LLVMConstReal (build-llvm-type t env) value)]
          [(sham:expr:si-value md value t)
           (LLVMConstInt (build-llvm-type t env) (cast value _sint64 _uint64) #f)]
          [(sham:expr:ui-value md value t)
           (LLVMConstInt (build-llvm-type t env) value #f)]
          [(sham:expr:sizeof md type)
           (define llvm-type (build-llvm-type type env))
           (LLVMConstInt (internal-type-jit (env-type-prim (env-lookup 'i32 env)))
                         (LLVMStoreSizeOfType jit-target-data
                                              llvm-type)
                         #f)]
          [(sham:expr:type md t)
           (build-llvm-type t env)]
          [(sham:expr:void md) (void)]
          [(sham:expr:gep md ptr indxs)
           (LLVMBuildGEP jit-builder
                         (build-expression ptr env)
                         (map (curryr build-expression env) indxs)
                         "gep")]
          [(sham:expr:var md sym)
           (LLVMBuildLoad jit-builder
                          (env-jit-value-ref (env-lookup sym env))
                          (symbol->string sym))]
          [(sham:expr:external md lib-id sym t)
           (define value
             (LLVMAddGlobal jit-module (build-llvm-type t env) (symbol->string sym)))
           (add-mapping! (list sym lib-id value))
           (LLVMBuildLoad jit-builder value (symbol->string sym))]
          [(sham:expr:global md sym)
           (env-jit-value-ref (env-lookup sym env))]
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
           (LLVMBuildCall jit-builder ref rand-values (substring s 0 3))]
          [(sham:rator:external lib-id id ret-type)
           (define s (symbol->string id))
           (define fn-type (LLVMFunctionType (build-llvm-type ret-type env)
                                             (map LLVMTypeOf rand-values) #f))
           (define fn-value (LLVMAddFunction jit-module s fn-type))
           (add-mapping! (list id lib-id fn-value))
           (LLVMBuildCall jit-builder fn-value rand-values (substring s 0 3))]
          [(sham:rator:symbol sym)
           (match (env-lookup sym env)
             [(env-jit-function ref type)
              (LLVMBuildCall jit-builder ref rand-values
                             (substring (symbol->string sym) 0 3))]
             [(env-jit-intr-function builder)
              (builder jit-builder rand-values)]
             [else (error "cannot figure out how to apply: ~a" rator)])]))
      (define entry-block (new-block 'entry))
      (LLVMPositionBuilderAtEnd jit-builder entry-block)
      (define new-env
        (for/fold ([env env])
                  ([arg args]
                   [type types]
                   [i (in-range (length args))])
          (define l-type (build-env-type type env))
          (define p-arg (LLVMBuildAlloca jit-builder
                                         (internal-type-jit (env-type-prim l-type))
                                         (symbol->string arg)))
          (LLVMBuildStore jit-builder (LLVMGetParam fref i) p-arg)
          (env-extend arg (env-jit-value p-arg l-type) env)))

      (build-statement body new-env)
      env-function)
    ;; TODO return values of name and obj
    (match def
      [(sham:def:type info type-name t)
       ;types are created in register phase
       ; something for recursive struct types to be done
       ;; TODO for struct switch to creating empty struct type
       ;and then adding fields here
       (env-extend type-name (env-lookup type-name env) module-env)]
      [(sham:def:function info function-name args types ret-type body)
       (printf "compiling-function ~a\n" function-name)
       (compile-function-definition function-name args types ret-type body)
       (define f (env-lookup function-name env))
       ;; TODO figure out info for attrs and passes together
       ;; and metadata for function as well, similar to info for module
       ;; (for ([attr attrs])
       ;;   (LLVMAddAttributeAtIndex
       ;;    (env-jit-function-ref f)
       ;;    (cast -1 _int _uint)
       ;;    (jit-lookup-attr attr context)))
       ;; (jit-run-function-pass passes jit-module f)
       (env-extend function-name f module-env)]
      [(sham:def:global info id t)
       (define env-value (env-lookup id env))
       (env-extend id env-value module-env)]))
  (match m
    [(sham:module info defs)
     (define env
       (for/fold ([env (create-initial-environment context)])
                 ([def defs])
         (printf "registering def: ~a\n" (sham:def-id def))
         (register-module-define def env)))
     (define module-env
       (for/fold ([module-env (empty-env)])
                 ([def defs])
         (compile-module-define def env module-env)))
     ;(jit-run-module-pass (cdr (assoc 'passes info)) jit-module)
     ;(LLVMVerifyModule jit-module 'LLVMPrintMessageAction #f)
     (jit-add-info module-env
                   (build-info info `((jit-module . ,jit-module)
                                      (ffi-mappings . ,(unbox ffi-mappings)))))]))


;; disabling tests for now
;; (module+ test
;;   (require racket/unsafe/ops)
;;   (require rackunit)
;;   (require disassemble)
;;   (require "../disassemble/disassemble/main.rkt")

;;   (define i32 (sham:type:ref 'i32))
;;   (define i64 (sham:type:ref 'i64))
;;   (define icmp-eq (sham:rator:symbol 'icmp-eq))
;;   (define urem (sham:rator:symbol 'urem))
;;   (define (build-app rator . rands)
;;     (sham:expr:app rator rands))
;;   (define (ui32 v)
;;     (sham:expr:ui-value v i32))
;;   (define ret sham:stmt:return)
;;   (define v sham:expr:var)
;;   (define rs sham:rator:symbol)
;;   (define (defn n args types ret-type stmt) (sham:def:function n '() '() args types ret-type stmt))
;;   (define module-env
;;     (compile-module
;;      (sham:module
;;       '((passes . (AlwaysInliner))
;;         (ffi-libs . ((libc . ("/usr/lib/libc" "6")))))
;;        (list
;;         (sham:def:type 'int i32)
;;         (sham:def:type 'int* (sham:type:pointer i32))
;;         (sham:def:type 'bc (sham:type:struct '(b c) (list i32 i32)))
;;         (sham:def:type 'pbc (sham:type:pointer (sham:type:ref 'bc)))
;;         (sham:def:type 'ui (sham:type:struct '(bc1 bc2) (list (sham:type:ref 'pbc)
;;                                                               (sham:type:ref 'pbc))))

;;         (sham:def:global 'constant1 i32)
;;         (sham:def:function 'setglobal '() '() '() '() i32
;;                            (sham:stmt:block
;;                             (list
;;                              ;; (sham:stmt:set! (sham:expr:global 'constant1)
;;                              ;;                 ;(build-app (sham:rator:symbol 'fact) (sham:expr:ui-value 5 i32))
;;                              ;;                 (sham:expr:ui-value 1 i32)
;;                              ;;                 )
;;                              (sham:stmt:set! (sham:expr:var 'constant1)
;;                                              (build-app (sham:rator:symbol 'fact) (sham:expr:ui-value 5 i32)))
;;                              (sham:stmt:return (sham:expr:ui-value 1 i32)))))
;;         (sham:def:function 'const1 '() '() '() '() i32
;;                            (sham:stmt:block
;;                             (list
;;                              (sham:stmt:set! (sham:expr:var 'constant1)
;;                                              (sham:expr:ui-value 2 i32))
;;                              (sham:stmt:return (sham:expr:var 'constant1)))))
;;         (sham:def:function 'id '() '() '(x) (list i32) i32
;;                            (ret (v 'x)))

;;         (defn
;;          'even? '(x) (list i32) i32
;;          (sham:stmt:if (build-app icmp-eq (build-app urem (sham:expr:var 'x) (ui32 2))
;;                                   (ui32 0))
;;                        (ret (ui32 1))
;;                        (ret (ui32 0))))

;;         (defn
;;          'meven? '(x) (list i32) i32
;;          (sham:stmt:if (build-app icmp-eq (v 'x) (ui32 0))
;;                        (ret (ui32 1))
;;                        (ret (build-app (rs 'modd?)
;;                                        (build-app (rs 'sub) (v 'x) (ui32 1))))))
;;         (defn
;;          'modd?  '(x) (list i32) i32
;;          (sham:stmt:if (build-app icmp-eq (v 'x) (ui32 0))
;;                        (ret (ui32 0))
;;                        (ret (build-app (rs 'meven?)
;;                                        (build-app (rs 'sub) (v 'x) (ui32 1))))))

;;         (defn 'fact '(x) (list i32) i32
;;           (sham:stmt:if (build-app icmp-eq (v 'x) (ui32 0))
;;                         (ret (ui32 1))
;;                         (ret (build-app (rs 'mul)
;;                                         (v 'x)
;;                                         (build-app (rs 'fact)
;;                                                    (build-app (rs 'sub)
;;                                                               (v 'x) (ui32 1)))))))

;;         (defn 'factr '(x) (list i32) i32
;;           (sham:stmt:let
;;            '(result i) (list i32 i32) (list (ui32 1) (ui32 1))
;;            (sham:stmt:block
;;             (list
;;              (sham:stmt:while
;;               (build-app (rs 'icmp-ule) (v 'i) (v 'x))
;;               (sham:stmt:block
;;                (list
;;                 (sham:stmt:set! (v 'result)
;;                                 (build-app (rs 'mul-nuw)
;;                                            (v 'result) (v 'i)))
;;                 (sham:stmt:set! (v 'i)
;;                                 (build-app (rs 'add-nuw) (v 'i) (ui32 1))))))
;;              (ret (v 'result))))))

;;         (defn 'sum-array '(arr size) (list (sham:type:ref 'int*) i32) i32
;;           (sham:stmt:let
;;            '(i sum) (list i32 i32) (list (ui32 0) (ui32 0))
;;            (sham:stmt:block
;;             (list
;;              (sham:stmt:while
;;               (build-app (rs 'icmp-ult) (v 'i) (v 'size))
;;               (sham:stmt:block
;;                (list
;;                 (sham:stmt:let '(arri) (list (sham:type:ref 'int*))
;;                                (list (sham:expr:gep (v 'arr)
;;                                                    (list (v 'i))))
;;                                (sham:stmt:set! (v 'sum)
;;                                                (build-app (rs 'add)
;;                                                           (v 'sum)
;;                                                           (build-app (rs 'load) (v 'arri)))))
;;                 (sham:stmt:set! (v 'i) (build-app (rs 'add) (v 'i) (ui32 1))))))
;;              (ret (v 'sum))))))
;;         (defn 'if-void-test '() '() i32
;;           (sham:stmt:block
;;            (list
;;             (sham:stmt:if (build-app icmp-eq (ui32 0) (ui32 0))
;;                           (sham:stmt:exp-stmt (sham:expr:void-value)
;;                                               (sham:stmt:void))
;;                           (sham:stmt:return (ui32 1)))
;;             (ret (ui32 0)))))
;;         (defn 'random '() '() i32
;;           (ret (sham:expr:app (sham:rator:external 'libc 'random i32) '())))
;;         (defn 'test '() '() (sham:type:ref 'void*)
;;           (ret (sham:expr:external 'libc 'random (sham:type:ref 'void*))))
;;         (defn 'malloc-test '() '() i32
;;           (sham:stmt:let '(ptr)
;;                          (list (sham:type:ref 'int*))
;;                          (list (build-app (rs 'malloc) (sham:expr:type (sham:type:ref 'int))))
;;                          (sham:stmt:block
;;                           (list
;;                            (sham:stmt:exp-stmt (build-app (rs 'store!) (ui32 42) (v 'ptr))
;;                                                (sham:stmt:void))
;;                            (ret (build-app (rs 'load) (v 'ptr)))))))))))

;;   ;; (jit-dump-module module-env)

;;   (jit-optimize-module module-env #:opt-level 3)
;;   (jit-dump-module module-env)
;;   (printf "verifying module: ~a\n" (jit-verify-module module-env))
;;   (define cenv (initialize-jit module-env #:opt-level 3))

;;   (define factr (jit-get-function 'factr cenv))
;;   (define fact (jit-get-function 'fact cenv))
;;   (define even? (jit-get-function 'even? cenv))
;;   (define meven? (jit-get-function 'meven? cenv))
;;   (define malloc-test (jit-get-function 'malloc-test cenv))
;;   (define sum-array (jit-get-function 'sum-array cenv))
;;   (define r (jit-get-function 'random cenv))
;;   (define test (jit-get-function 'test cenv))
;;   ;; (disassemble-ffi-function (jit-get-function-ptr 'random cenv)
;;   ;;                           #:size 70)
;;   (printf "running tests\n")
;;   (check-eq? (fact 5) 120)
;;   (check-eq? (factr 5) 120)
;;   (check-eq? (even? 42) 1)
;;   (check-eq? (meven? 21) 0)
;;   (check-eq? (malloc-test) 42)
;;   (check-eq? (sum-array (list->cblock '(1 2 3) _uint) 3) 6)
;;   (printf "random-value: ~a\n" (r)))
