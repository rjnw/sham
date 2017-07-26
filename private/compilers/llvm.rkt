#lang racket

(require "../ast/llvm.rkt")
(require "../llvm/ffi/all.rkt")
(define (empty-env)
  (make-immutable-hash))
(define (env-extend key val env)
  (hash-set env key val))
(define (env-lookup key env)
  (hash-ref env key))
(define (dump-env env)
  (LLVMDumpModule (env-lookup '#%module env))
  (pretty-display env))

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
     (define (compile-type-declaration t)
       (void))
     (define (compile-function-type arg-types ret-type env)
       (void))
     (define (compile-function-declaration ftype name)
       (void))

     (define decl-env
       (for/fold [(env (init-type-env llvm-context))]
                 [(defn defns)]
         (match defn
           ([llvm:defn:function name _ arg-types ret-type _ _ _]
            (define ftype (compile-function-type arg-types ret-type env))
            (env-extend name (compile-function-declaration ftype name) env))
           ([llvm:defn:type name t]
            (env-extend name (compile-type-declaration t) env)))))


     (define (compile-type t)
       (match t
         ([llvm:type:label sym]
          (env-lookup sym decl-env))
         ([llvm:type:struct fields]
          (LLVMStructType (map compile-type fields) 0))
         ([llvm:type:function args ret]
          (LLVMFunctionType (map compile-type args) (compile-type ret) #f))
         ([llvm:type:pointer to]
          (LLVMPointerType (compile-type to) 0))))
     (define (compile-function fun)
       (void))
     (define (compile-module-defn defn env)
       (match defn
         [(llvm:defn:type name t)
          (env-extend name (compile-type t) env)]
         [(? llvm:defn:function?)
          (env-extend (llvm:defn:function-name defn) (compile-function defn) env)]))
     (define defn-env
       (for/fold [(env (empty-env))]
                 [(defn defns)]
         (compile-module-defn defn env)))
     (run-module-passes passes llvm-module)
     (LLVMVerifyModule llvm-module 'LLVMPrintMessageAction #f)
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
  (dump-env env0))
