#lang racket

(require sham/ir/ast/syntax
         sham/ir/ast/op
         sham/ir/ast/simple
         sham/ir/ast/specific
         sham/ir/optimize
         sham/ir/builder
         sham/llvm/ir/md
         (prefix-in llvm- sham/llvm/ir/simple))

(define identity-f
  (function^ (identity [i : i64] : i64)
             (return^ i)))
(define pow-f
  (function^ (pow [x : i64] [n : i64] : i64)
             (if^ (icmp-ule^ n (ui64 0))
                  (return^ x)
                  (return^ (mul^ x (app^ 'pow x (sub-nuw^ n (ui64 1))))))))

(module+ test
  (require rackunit
           sham/ir/env
           sham/ir/dump
           sham/ir/verify)
  (define t-module
    (module^ raw-sham-function-test-module
             [identity-f pow-f]))
  (define s-mod (build-sham-module t-module))
  (define s-env (build-sham-env t-module))
  (sham-dump-llvm-ir s-env)
  (sham-env-optimize! s-env #:opt-level 2)
  (sham-dump-llvm-ir s-env)
  (test-true "sham-raw-functions" (sham-verify-llvm-ir s-env)))
