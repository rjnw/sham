#lang racket

(require sham/md
         sham/ir
         (prefix-in ll- sham/llvm/ir))

(provide (all-defined-out))

(define identity-f
  (make-def-function
              'identity
              (ll-make-type-function (list i32) #f i32)
              (stmt-expr (make-expr-let `(v) (list (ll-val-param 0)) (list i32)
                             (stmt-return (expr-ref 'v))
                             (expr-void)))))
(define pow-f
  (make-def-function
   'pow
   (ll-make-type-function (list i64 i64) #f i64)
   (stmt-expr
    (make-expr-let `(x n) (list (ll-val-param 0) (ll-val-param 1)) (list i64 i64)
                   (stmt-if (op-icmp-ule (expr-ref 'n) (ui64 0))
                            (stmt-return (ui64 1))
                            (stmt-return (op-mul
                                          (expr-ref 'x)
                                          (expr-app 'pow (expr-ref 'x)
                                                    (op-sub-nuw
                                                     (expr-ref 'n) (ui64 1))))))
                   (expr-void)))))

(module+ test
  (require rackunit
           sham/ir/dump
           sham/ir/verify)
  (define t-module
    (make-def-module 'raw-sham-function-test-module (list identity-f pow-f)))
  (define s-env (build-sham-env t-module))
  (sham-dump-llvm-ir s-env)
  (sham-env-optimize-llvm! s-env #:opt-level 2)
  (sham-dump-llvm-ir s-env)
  (test-true "sham:verify:raw-functions" (sham-verify-llvm-ir s-env)))
