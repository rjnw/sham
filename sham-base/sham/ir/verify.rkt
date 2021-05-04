#lang racket

(require sham/ir/env
         sham/llvm/ir/verify)

(provide (all-defined-out))

(define (sham-verify-llvm-ir s-env)
  (verify-llvm-module (sham-env-ll-env s-env)))

(define (sham-verify-llvm-ir-error senv)
  (unless (sham-verify-llvm-ir senv)
    (error 'sham:ir "unable to verify llvm ir for sham module")))
