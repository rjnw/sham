#lang racket
(require ffi/unsafe)

(require "llvm/ffi/all.rkt"
         "llvm/adjunct.rkt"
         "llvm/pass-table.rkt")

(require "env.rkt"
         "init.rkt"
         "types.rkt"
         "ast.rkt"
         "module-env.rkt"
         "rator.rkt"
         "dump.rkt"
         "utils.rkt")

(provide compile-module
         jit-get-function
         jit-get-function-ptr
         jit-get-racket-type
         jit-verify-module)

(define diagnose-compile (make-parameter #f))
(define data-layout (make-parameter #f))

(define (mcjit-function-ptr f-sym mod-env)
  (define mcjit-engine (env-get-mcjit mod-env))
  (unless mcjit-engine
    (error "mcjit not initialized"))
  (cast (LLVMGetFunctionAddress mcjit-engine (symbol->string f-sym)) _uint64 _pointer))

(define (jit-get-function f-sym mod)
  (define fptr (jit-get-function-ptr f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
  (cast fptr _pointer f-type))

(define (orc-function-ptr f-sym mod)
  (define-values (error-code target-address)
    (LLVMOrcGetSymbolAddress (env-get-orc mod) (symbol->string f-sym)))
  ;; (printf "error code: ~a\n" error-code)
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

(define (jit-cleanup mod-env)
  ;; TODO dispose module, cleanup for orc
  ;; LLVMDisposeModule
  ;; LLVMOrcDisposeInstance
  ;; LLVMOrcDisposeSharedModule
  ;; LLVMOrcRemoveModule
  (void))

(define (register-module-define stmt llvm-module env)
    (define (compile-function-declaration env-function-type function-name)
      (define function-type
        (internal-type-jit (env-type-prim env-function-type)))
      (LLVMAddFunction llvm-module (symbol->string function-name) function-type))

    (match stmt
      [(sham:def:type info type-name t)
       (env-extend type-name (build-env-type t env) env)]
      [(sham:def:function info function-name args types ret-type body)
       (define type (build-env-type (sham:ast:type:function types ret-type) env))
       (define function-obj (compile-function-declaration type function-name))
       (env-extend function-name (env-jit-function function-obj type) env)]
      [(sham:def:global info id t)
       (define type (build-env-type t env))
       (define value
         (LLVMAddGlobal llvm-module
                        (internal-type-jit (env-type-prim type))
                        (symbol->string id)))
       (LLVMSetInitializer
        value
        (LLVMConstNull (internal-type-jit (env-type-prim type))))
       (env-extend id (env-jit-value value (build-env-type t env)) env)]))

(define (compile-module-define def builder llvm-module module-env)
  (define env (env-get-top-env module-env))
  (define ffi-mappings (env-get-ffi-mappings module-env))
  (define rkt-mappings (env-get-rkt-mappings module-env))
  (define function-info-map (env-get-per-function-info-map module-env))
  (define type-info-map (env-get-per-type-info-map module-env))
  (define add-rkt-mapping! (curry hash-set! rkt-mappings))
  (define add-ffi-mapping! (curry hash-set! ffi-mappings))
  (define add-function-info! (curry hash-set! function-info-map))
  (define add-type-info! (curry hash-set! type-info-map))
  (define intrinsic-map (make-hash))
  (define (compile-function-definition name args types ret-type body)
    (define env-function (env-lookup name env))
    (define fref (env-jit-function-ref env-function))
    (define ftype (env-jit-function-type env-function))
    (define jit-target-data
      (LLVMCreateTargetData (LLVMGetDataLayout llvm-module)))
    (define (build-llvm-type t env)
      (internal-type-jit (compile-type t env)))
    (define (new-block n)
      (LLVMAppendBasicBlockInContext (LLVMGetModuleContext llvm-module)
                                     fref (symbol->string n)))
    (define alloca-block (new-block 'alloca))
    (define entry-block (new-block 'entry))
    (LLVMPositionBuilderAtEnd builder entry-block)
    (define (add-alloca type sym)
      (define curr-block (LLVMGetInsertBlock builder))
      (LLVMPositionBuilderAtEnd builder alloca-block)
      (define val (LLVMBuildAlloca builder type (symbol->string sym)))
      (LLVMPositionBuilderAtEnd builder curr-block)
      val)
    (define new-env
      (for/fold ([env env])
                ([arg args]
                 [type types]
                 [i (in-range (length args))])
        (define l-type (build-env-type type env))
        (define p-arg
          (LLVMBuildAlloca builder
                           (internal-type-jit (env-type-prim l-type))
                           (symbol->string arg)))
        (LLVMBuildStore builder (LLVMGetParam fref i) p-arg)
        (env-extend arg (env-jit-value p-arg l-type) env)))

    (define (build-statement stmt env)
      (match stmt
        [(sham:ast:stmt:set! md lhs v) ;;TODO: right now lhs can only be a var
         (match lhs
           [(sham:ast:expr:var md var)
            (define lhs-v (env-jit-value-ref (env-lookup var env)))
            (define rhs-v (build-expression v env))
            (LLVMBuildStore builder rhs-v lhs-v)]
           [(sham:ast:expr:global md var)
            (define global-v (env-jit-value-ref (env-lookup var env)))
            (define rhs-v (build-expression v env))
            (LLVMSetInitializer global-v rhs-v)]
           [else (error "sham:ast:stmt:set! only supports a variable in lhs.")])]
        [(sham:ast:stmt:if md tst thn els)
         (define tst-value (build-expression tst env))
         (define then-block (new-block 'then))
         (define else-block (new-block 'else))
         (define end-block (new-block 'ife))
         (env-extend '#%break-block end-block env)

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

        [(sham:ast:stmt:switch md tst checks cases default)
         (define prev-block (LLVMGetInsertBlock builder))
         (define switch-entry (new-block 'switch-entry))
         (define switch-default (new-block 'switch-default))
         (define afterswitch-block (new-block 'afterswitch-block))

         (define tst-value (build-expression tst env))

         (define env (env-extend '#%break-block afterswitch-block env))
         (LLVMPositionBuilderAtEnd builder switch-default)
         (build-statement default env)
         (LLVMBuildBr builder afterswitch-block)

         (define switch (LLVMBuildSwitch builder tst-value switch-default (length cases)))

         (for ([val checks]
               [stmt cases])
           (define ve (build-expression val))
           (define case-block (new-block 'switch-case))
           (LLVMPositionBuilderAtEnd case-block)
           (build-statement stmt)
           (LLVMAddCase switch ve case-block))
         (LLVMPositionBuilderAtEnd builder switch-default)]

        [(sham:ast:stmt:while md tst body)
         (define prev-block (LLVMGetInsertBlock builder))
         (define loop-entry (new-block 'loop-entry))
         (define loop-block (new-block 'loop-block))
         (define afterloop-block (new-block 'afterloop-block))
         (env-extend '#%break-block afterloop-block env)
         (LLVMBuildBr builder loop-entry)

         (LLVMPositionBuilderAtEnd builder loop-entry)
         (define tst-value (build-expression tst env))
         (LLVMBuildCondBr builder tst-value loop-block afterloop-block)

         (LLVMPositionBuilderAtEnd builder loop-block)

         (build-statement body env)
         (define body-end-blk (LLVMGetInsertBlock builder))
         (LLVMBuildBr builder loop-entry)

         (LLVMPositionBuilderAtEnd builder afterloop-block)]

        [(sham:ast:stmt:break md)
         (LLVMBuildBr builder (env-lookup '#%break-block env))]

        [(sham:ast:stmt:return md v)
         (if (sham:ast:expr:void? v)
             (LLVMBuildRetVoid builder)
             (LLVMBuildRet builder (build-expression v env)))]

        [(sham:ast:stmt:block md stmts)
         (for ([stmt stmts])
           (build-statement stmt env))]
        [(sham:ast:stmt:void md) (void)]
        [(sham:ast:stmt:expr md e) (build-expression e env)]
        [else (error "unknown statement while compiling sham" stmt)]))

    (define (build-expression e env)
      (match e
        [(sham:ast:expr:let md ids types vals st ex)
         (define new-env
           (for/fold ([env env])
                     ([id-type types]
                      [id ids]
                      [val vals])
             (define val-type (build-llvm-type id-type env))
             ;; TODO add llvm lifetime metadata at the end of let
             (define val-p (add-alloca val-type id))
             (unless (sham:ast:expr:void? val)
               (LLVMBuildStore builder (build-expression val env) val-p))
             (env-extend id
                         (env-jit-value val-p (env-type val-type id-type))
                         env)))
         (build-statement st new-env)
         (build-expression ex new-env)]
        [(sham:ast:expr:app md rator rands)
         (define rand-values (map (curryr build-expression env) rands))
         (build-app rator rand-values env)]
        [(sham:ast:expr:const:fl md value t)
         (LLVMConstReal (build-llvm-type t env) value)]
        [(sham:ast:expr:const:si md value t)
         (LLVMConstInt (build-llvm-type t env) (cast value _sint64 _uint64) #f)]
        [(sham:ast:expr:const:ui md value t)
         (LLVMConstInt (build-llvm-type t env) value #f)]
        [(sham:ast:expr:const:llvm md v t) v]
        [(sham:ast:expr:const:struct md vals t)
         (LLVMConstStruct (map (curryr build-expression env) vals))]
        [(sham:ast:expr:const:array md vals t)
         (LLVMConstArray (build-llvm-type t env)
                         (map (curryr build-expression env) vals))]
        [(sham:ast:expr:const:string md str)
         (LLVMBuildGlobalStringPtr builder str (symbol->string (gensym 'str)))]
        [(sham:ast:expr:const:vector md vals)
         (LLVMConstVector (map (curryr build-expression env) vals))]
        [(sham:ast:expr:sizeof md type)
         (define llvm-type (build-llvm-type type env))
         (LLVMConstInt (internal-type-jit (env-type-prim (env-lookup 'i32 env)))
                       (LLVMStoreSizeOfType jit-target-data
                                            llvm-type)
                       #f)]
        [(sham:ast:expr:etype md t) (build-llvm-type t env)]
        [(sham:ast:expr:void md) (void)]
        [(sham:ast:expr:gep md ptr indxs)
         (LLVMBuildGEP builder
                       (build-expression ptr env)
                       (map (curryr build-expression env) indxs)
                       "gep")]
        [(sham:ast:expr:var md sym)
         (define env-value (env-lookup sym env))
         (match env-value
           [(env-jit-value jv t)
            (LLVMBuildLoad builder jv (symbol->string sym))]
           [(env-jit-function jv t)
            jv])]
        [(sham:ast:expr:external md lib-id sym t)
         (define value
           (LLVMAddGlobal llvm-module
                          (build-llvm-type t env)
                          (symbol->string sym)))
         (add-ffi-mapping! sym (cons lib-id value))
         (LLVMBuildLoad builder value (symbol->string sym))]
        [(sham:ast:expr:global md sym) (env-jit-value-ref (env-lookup sym env))]
        [else (error "unknown experssion while compiling sham" e)]))

    (define (build-app rator rand-values env)
      (match rator
        [(sham:ast:rator:intrinsic md str-id ret-type)
         (define s (symbol->string str-id))
         (define ref
           (if (hash-has-key? intrinsic-map s)
               (hash-ref intrinsic-map s)
               (let* ([fn-type (LLVMFunctionType (build-llvm-type ret-type env)
                                                 (map LLVMTypeOf rand-values) #f)]
                      [ref (LLVMAddFunction llvm-module s fn-type)])
                 (hash-set! intrinsic-map s ref)
                 ref)))
         (LLVMBuildCall builder ref rand-values (llvm-lhs s))]
        [(sham:ast:rator:external md lib-id id ret-type)
         #:when (hash-has-key? ffi-mappings id)
         (LLVMBuildCall builder (cdr (hash-ref ffi-mappings id)) rand-values "e")]
        [(sham:ast:rator:external md lib-id id ret-type)
         (define s (symbol->string id))
         (define fn-type (LLVMFunctionType (build-llvm-type ret-type env)
                                           (map LLVMTypeOf rand-values) #f))
         (define fn-value (LLVMAddFunction llvm-module s fn-type))
         (add-ffi-mapping! id (cons lib-id fn-value))
         (LLVMBuildCall builder fn-value rand-values (llvm-lhs s))]
        [(sham:ast:rator:racket md id rkt-fun type)
         (define s (symbol->string id))
         (define ct (compile-type type env))
         (define fn-type (internal-type-jit ct))
         (define fn-value (LLVMAddFunction llvm-module s fn-type))
         (add-rkt-mapping! s (list rkt-fun (internal-type-racket ct) fn-value))
         (LLVMBuildCall builder fn-value rand-values (llvm-lhs s))]
        [(sham:ast:rator:symbol md sym)
         (if (env-contains? sym env)
             (match (env-lookup sym env)
               [(env-jit-function ref type)
                (define call-name
                  (if (equal? (LLVMGetReturnType
                               (internal-type-jit (env-type-prim type)))
                              (LLVMVoidType))
                      ""
                      (llvm-lhs sym)))
                (LLVMBuildCall builder ref rand-values call-name)]
               [(env-jit-intr-function appbuilder)
                (appbuilder builder rand-values)]
               [(env-jit-value jit-value (env-type (sham:ast:type:pointer _ (sham:ast:type:function _ _ _)) it))
                (LLVMBuildCall builder (LLVMBuildLoad builder jit-value "ptrrator") rand-values "ptrcall")]
               [v (error "cannot figure out how to apply " rator v)])
             (error "rator symbol not found in module " sym))]))
    (build-statement body new-env)
    (LLVMPositionBuilderAtEnd builder alloca-block)
    (LLVMBuildBr builder entry-block)
    env-function)

  ;; TODO return values of name and obj
  (match def
    [(sham:def:type info type-name t)
     ;types are created in register phase
     ; something for recursive struct types to be done
     ;; TODO for struct switch to creating empty struct type
     ;and then adding fields here,
     ;; for now we can use opaque pointer and then cast everytime HACK!!
     (add-type-info! type-name info)
     ;; (env-extend type-name (env-lookup type-name env) module-env)
     (values type-name (env-lookup type-name env))]
    [(sham:def:function info function-name args types ret-type body)
     (compile-function-definition function-name args types ret-type body)
     (define f (env-lookup function-name env))
     (define lf (env-jit-function-ref f))
     (add-function-info! function-name info)
     ;; (env-extend function-name f module-env)
     (values function-name f)]
    [(sham:def:global info id t)
     (define env-value (env-lookup id env))
     ;; (env-extend id env-value module-env)
     (values id env-value)]))

(define (compile-module m [module-name "module"] [context (global-jit-context)])
  (when (diagnose-compile)
    (define (diag-handler dinfo voidp)
      (define diag-desc (LLVMGetDiagInfoDescription dinfo))
      (printf "llvm-diagnose: ~a\n" (cast diag-desc _pointer _string))
      (LLVMDisposeMessage diag-desc))
    (LLVMContextSetDiagnosticHandler context diag-handler #f))

  (define llvm-module (LLVMModuleCreateWithNameInContext module-name context))
  (when (data-layout) (LLVMSetDataLayout llvm-module (data-layout)))
  (LLVMSetTarget llvm-module (LLVMGetDefaultTargetTriple))
  (define builder (LLVMCreateBuilderInContext context))
  (define function-info-map (make-hash))
  (define type-info-map (make-hash))
  (define ffi-mappings (make-hash))
  (define rkt-mappings (make-hash))
  (match m
    [(sham:def:module info id defs)
     (define env
       (for/fold ([env (create-initial-environment context)])
                 ([def defs])
         (register-module-define def llvm-module env)))
     (for/fold ([module-env (env-add-info
                             (empty-env)
                             (update-info!
                              info
                              `((,top-env-key           . ,env)
                                (,module-key            . ,llvm-module)
                                (,context-key           . ,context)
                                (,per-function-info-key . ,function-info-map)
                                (,per-type-info-key     . ,type-info-map)
                                (,ffi-mapping-key       . ,ffi-mappings)
                                (,rkt-mapping-key       . ,rkt-mappings))))])
               ([def defs])
       (define-values (id value) (compile-module-define def builder llvm-module module-env))
       (env-extend id value module-env))]))
