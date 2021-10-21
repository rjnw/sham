#lang racket

(require sham/md
         sham/ir
         (prefix-in ll- sham/llvm/ir/simple))

(provide (all-defined-out))

(define identity-f
  (function (identity [i : i64] : i64)
             (stmt-return i)))
(define pow-f
  (function (pow [x : i64] [n : i64] : i64)
             (stmt-if (op-icmp-ule n (ui64 0))
                  (stmt-return (ui64 1))
                  (stmt-return (op-mul x (expr-app 'pow x (op-sub-nuw n (ui64 1))))))))

(module+ test
  (require rackunit)
  (define t-module
    (def-module 'raw-sham-function-test-module
      identity-f pow-f))
  (define s-env (build-sham-env t-module))
  (sham-dump-llvm-ir s-env)
  (sham-env-optimize-llvm! s-env #:opt-level 2)
  (sham-dump-llvm-ir s-env)
  (test-true "sham:verify:syntax-functions" (sham-verify-llvm-ir s-env)))
