#lang racket

(require "../ast/llvm.rkt")
(require "../llvm/ffi/all.rkt")

(define (empty-env)
  (make-immutable-hash))
(define (env-extend key val env)
  (hash-set env key val))
(define (env-lookup key env)
  (hash-ref env key))
(define (get-module env)
  (env-lookup '#%module env))
(define (dump-env env)
  (LLVMDumpModule (get-module env))
  (pretty-display env))
(define (llvm-verify-module env)
  (LLVMVerifyModule (get-module env) 'LLVMPrintMessageAction #f))

(define (run-module-passes passes llvm-module)
  (void))

(define (compile-llvm-ast module-ast llvm-context)
  (match module-ast
    [(llvm:module name layout target passes defns)
     (define (diag-handler dinfo voidp)
       (printf "\tllvm-diag: ~a\n" (LLVMGetDiagInfoDescription dinfo)))
     (LLVMContextSetDiagnosticHandler llvm-context diag-handler #f)
     (define llvm-module (LLVMModuleCreateWithNameInContext name llvm-context))
     (LLVMSetDataLayout llvm-module layout)
     (LLVMSetTarget llvm-module target)
     (define builder (LLVMCreateBuilderInContext llvm-context))
     (define (compile-type-declaration t env)
       (match t
         ([llvm:type:label sym]
          (env-lookup sym env))
         ([llvm:type:struct fields]
          (LLVMStructType (map (curryr compile-type-declaration env) fields) 0))
         ([llvm:type:function args ret]
          (LLVMFunctionType (compile-type-declaration ret env)
                            (map (curryr compile-type-declaration env) args)
                             #f))
         ([llvm:type:pointer to]
          (LLVMPointerType (compile-type-declaration to env) 0))))
     (define (compile-function-type arg-types ret-type env)
       (compile-type-declaration (llvm:type:function arg-types ret-type) env))
     (define (compile-function-declaration ftype name)
       (LLVMAddFunction llvm-module name ftype))

     (define decl-env
       (for/fold [(env (init-type-env llvm-context))]
                 [(defn defns)]
         (match defn
           ([llvm:defn:function name _ arg-types ret-type _ _ _]
            (define ftype (compile-function-type arg-types ret-type env))
            (env-extend name (compile-function-declaration ftype name) env))
           ([llvm:defn:type name t]
            (env-extend name (compile-type-declaration t env) env)))))

     (define (compile-type t name)
       ; TODO: do recursive struct types here
       (env-lookup name decl-env))

     (define (compile-function fun)
       (match-define (llvm:defn:function name arg-syms arg-types ret-tyep attrs passes blocks) fun)
       (define fref (env-lookup name decl-env))
       (define (compile-blocks blocks env)
         (define new-env
           (for/fold ([env env])
                     ([block blocks])
             (match-define (llvm:block label _) block)
             (env-extend label (LLVMAppendBasicBlockInContext llvm-context fref label) env)))
         (for/fold
             ([env new-env])
             ([block blocks])
           (match block
             [(llvm:block label stmts)
              (define block (env-lookup label env))
              (LLVMPositionBuilderAtEnd builder block)
              (for/fold ([env env])
                        ([stmt stmts])
                (compile-statement stmt env))])))
       (define (compile-expression expr env)
         (match expr
           [(llvm:expr:type sym)
            (env-lookup sym env)]
           ;; [(llvm:expr:calli sym exprs)
           ;;  ]
           [(llvm:expr:call sym exprs)
            (LLVMBuildCall builder (env-lookup sym env)
                           (map (curryr compile-expression env) exprs) "app")]
           [(llvm:expr:ui-value v type)
            (LLVMConstInt (compile-type-declaration type env) v #f)]
           [(llvm:expr:si-value v type)
            (LLVMConstInt (compile-type-declaration type env) v #t)]
           [(llvm:expr:fl-value v type)
            (LLVMConstReal (compile-type-declaration type env) v)]
           [(llvm:expr:sizeof type)
            (LLVMConstInt (env-lookup 'i32 env)
                          (LLVMStoreSizeOfType target (compile-type-declaration type env))
                          #f)]
           [(llvm:expr:gep expr indxs)
            (LLVMBuildGEP builder (compile-expression expr env)
                          (map (curryr compile-expression env) indxs)
                          "gep")]))
       (define (compile-statement stmt env)
         (match stmt
           [(llvm:stmt:set! sym expr)
            (env-extend sym (compile-expression expr env) env)]
           [(llvm:stmt:store! expr-t expr-v)
            (LLVMBuildStore builder
                            (compile-expression expr-t env)
                            (compile-expression expr-v env))
            env]
           [(llvm:stmt:cond-branch expr label-t label-e)
            (LLVMBuildCondBr builder (compile-expression expr env)
                             (env-lookup label-t env)
                             (env-lookup label-e env))
            env]
           [(llvm:stmt:branch label)
            (LLVMBuildBr builder (env-lookup label env))
            env]
           [(llvm:stmt:return expr)
            (LLVMBuildRet builder (compile-expression expr env))
            env]
           [(llvm:stmt:return-void)
            (LLVMBuildRetVoid builder)
            env]
           [else env]))
       (define new-env (for/fold ([env decl-env])
                                 ([arg arg-syms]
                                  [i (in-range (length arg-syms))])
                         (env-extend arg (LLVMGetParam fref i) env)))
       (compile-blocks blocks new-env)
       fref)
     (define (compile-module-defn defn env)
       (match defn
         [(llvm:defn:type name t)
          (env-extend name (compile-type t name) env)]
         [(? llvm:defn:function?)
          (env-extend (llvm:defn:function-name defn) (compile-function defn) env)]))
     (define defn-env
       (for/fold [(env (empty-env))]
                 [(defn defns)]
         (compile-module-defn defn env)))
     (run-module-passes passes llvm-module)
     (env-extend '#%module llvm-module defn-env)]))


(define (init-type-env llvm-context)
  (for/fold ([env (empty-env)])
            ([type `((i1 ,(LLVMInt1TypeInContext llvm-context))
                     (i8 ,(LLVMInt8TypeInContext llvm-context))
                     (i16 ,(LLVMInt16TypeInContext llvm-context))
                     (i32 ,(LLVMInt32TypeInContext llvm-context))
                     (i64 ,(LLVMInt64TypeInContext llvm-context))
                     (i128 ,(LLVMInt128TypeInContext llvm-context))
                     (f32 ,(LLVMFloatTypeInContext llvm-context))
                     (f64 ,(LLVMDoubleTypeInContext llvm-context))
                     (void ,(LLVMVoidTypeInContext llvm-context)))])
    (env-extend (first type) (second type) env)))

(module+ test
  (require rackunit)
  (define llvm-global-context (LLVMGetGlobalContext))
  (define env0
    (compile-llvm-ast
     (llvm:module "env0"
                  "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
                  "x86_64-unknown-linux-gnu"
                  `()
                  (list
                   (llvm:defn:type 'void*
                                   (llvm:type:pointer (llvm:type:label 'void)))))
     llvm-global-context))
  (llvm-verify-module env0)
  (dump-env env0)

  (define env1
    (compile-llvm-ast
     (llvm:module
      "env1"
      "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
      "x86_64-unknown-linux-gnu"
      `()
      (list
       (llvm:defn:function
        "voidf" '() '() (llvm:type:label 'void)
        '() '()
        (list (llvm:block "entry" (list (llvm:stmt:return-void)))))))
     llvm-global-context))
  ;; (llvm-verify-module env1)
  ;; (dump-env env1)
  )
