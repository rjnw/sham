#lang racket

(require "llvm/pass-table.rkt"
         "llvm/ffi/all.rkt"
         "info-key.rkt")

(provide run-function-pass
         run-module-pass)

(define function-pass-info-key 'function-passes)
(define module-pass-info-key 'module-passes)

;; function and module passes are both list of symbols
(define (run-function-pass lf jit-module context info)
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-module))
  (do-if-info-key
   function-pass-info-key info passes
   (for ([pass passes])
     ((lookup-pass pass) fpm)))
  (begin0
      (LLVMRunFunctionPassManager fpm lf)
    (LLVMDisposePassManager fpm)))

(define (run-module-pass jit-module context info)
  (define mpm (LLVMCreatePassManager jit-module))
  (do-if-info-key
   function-pass-info-key info passes
   (for ([pass passes])
     ((lookup-pass pass) mpm)))
  (begin0
      (LLVMRunPassManager mpm jit-module)
    (LLVMDisposePassManager mpm)))
