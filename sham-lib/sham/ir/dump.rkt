#lang racket

(require sham/ir/env
         sham/llvm/ir/dump)

(provide (all-defined-out))

(define (sham-dump-llvm-ir s-env)
  (dump-llvm-ir (sham-env-ll-env s-env)))

(define (sham-write-llvm-ir s-env fname)
  (write-llvm-ir (sham-env-ll-env s-env) fname))
