#lang racket

(require sham/ir
         (prefix-in ll- sham/llvm/ir/simple))

(provide (all-defined-out))

(define identity-f (function (id [x : i64] : i64) (stmt-return x)))

(define pow-f
  (function (pow [x : i64] [n : i64] : i64)
            (stmt-if (op-icmp-ule n (ui64 0))
                     (stmt-return (ui64 1))
                     (stmt-return (op-mul x (expr-app 'pow x (op-sub-nuw n (ui64 1))))))))

(define (gen-pow-f n)
  (make-def-function
     (format "pow-~a" n)
     (ll-type-function i64 #f i64)
     (stmt-return (for/fold ([result (ui64 1)])
                            ([i n])
                    (op-mul (expr-ref 0) result)))))

(define pow-2 (gen-pow-f 2))

(module+ test
  (require rackunit
           sham/parameters)
  (define t-module
    (make-def-module 'raw-sham-function-test-module (list identity-f pow-f pow-2)))
  (define s-env (parameterize ([debug-sham-builder #t]) (build-sham-env t-module)))
  (sham-dump-llvm-ir s-env)
  (test-true "sham:verify:raw-functions" (sham-verify-llvm-ir s-env)))
