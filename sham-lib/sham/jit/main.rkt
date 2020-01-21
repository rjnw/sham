#lang racket
(require ffi/unsafe)

(require sham/llvm
         sham/ast/core
         sham/env
         sham/ir/types)

(provide jit-get-function
         jit-get-function-ptr
         jit-get-racket-type)

(define (mcjit-function-ptr f-sym mod-env)
  (define mcjit-engine (env-get-mcjit mod-env))
  (unless mcjit-engine
    (error "mcjit not initialized"))
  (cast (LLVMGetFunctionAddress mcjit-engine (symbol->string f-sym)) _uint64 _pointer))

(define (jit-get-function f-sym mod)
  (define fptr (jit-get-function-ptr f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (internal-type-racket (env-type-prim (env-function-type fref))))
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

(define (jit-cleanup mod-env)
  ;; TODO dispose module, cleanup for orc
  ;; LLVMDisposeModule
  ;; LLVMOrcDisposeInstance
  ;; LLVMOrcDisposeSharedModule
  ;; LLVMOrcRemoveModule
  (void))
