#lang racket

(require sham/llvm/ir/env
         sham/llvm/ffi)

(provide verify-llvm-module)

(define (verify-llvm-module mod-env)
  (define llvm-mod-ref (llvm-env-module-ref mod-env))
  (LLVMVerifyModule llvm-mod-ref 'LLVMPrintMessageAction #f))
