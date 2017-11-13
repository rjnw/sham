#lang racket
(require "llvm/ffi/all.rkt"
         "info-key.rkt"
         "env.rkt")

(define (jit-optimize-function mod-env #:opt-level [level 1])
  (define jit-mod (env-get-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))

;;we don't want rely on llvm's basic optimization levels anymore
;; as they are based on c style languages. need to figure out our own
(define (jit-optimize-module mod-env #:opt-level [level 1])
  (define jit-mod (env-get-module mod-env))
  (define mpm (LLVMCreatePassManager))
  (define pmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel pmb level)
  (LLVMPassManagerBuilderSetSizeLevel pmb 100)
  (LLVMPassManagerBuilderPopulateModulePassManager pmb mpm)
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)
    (LLVMPassManagerBuilderDispose pmb)))
