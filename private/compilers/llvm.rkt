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
       (match-define (llvm:defn:function name arg-syms arg-types ret-tyep attrs passes stmt) fun)
       (define fref (env-lookup name decl-env))
       (define (compile-statement stmt env)
         (match stmt
           [(llvm:stmt:block label stmts)
            (LLVMAppendBasicBlockInContext llvm-context fref label)
            (for/fold ([env env])
                      ([stmt stmts])
              (compile-statement stmt env))
            env]
           [(llvm:stmt:return-void)
            (LLVMBuildRetVoid builder)
            env]
           [else env]))
       (define new-env (for/fold ([env decl-env])
                                 ([arg arg-syms]
                                  [i (in-range (length arg-syms))])
                         (env-extend arg (LLVMGetParam fref i) env)))
       (compile-statement stmt new-env)
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
     (llvm:module "env1"
                  "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
                  "x86_64-unknown-linux-gnu"
                  `()
                  (list
                   (llvm:defn:function "empty" '() '() (llvm:type:label 'void)
                                       '() '()
                                       (llvm:stmt:block "entry"
                                                        (list (llvm:stmt:return-void))))))
     llvm-global-context))
  (llvm-verify-module env1)
  (dump-env env1))
