#lang racket

(require sham/llvm
         sham/env)

(provide verify-llvm-module)

(define (verify-llvm-module mod-env)
  (define llvm-mod (env-get-llvm-module mod-env))
  (LLVMVerifyModule llvm-mod 'LLVMPrintMessageAction #f))
