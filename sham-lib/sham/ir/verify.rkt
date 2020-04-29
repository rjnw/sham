#lang racket

(require sham/ir/env
         sham/llvm/ir/verify)

(provide (all-defined-out))

(define (sham-verify-llvm-ir s-env)
  (verify-llvm-module (sham-env-ll-env s-env)))
