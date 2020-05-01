#lang racket

(require sham/ir/ast
         sham/ir/optimize
         sham/ir/builder
         sham/llvm/ir/md
         (prefix-in llvm- sham/llvm/ir/simple))

(provide (all-defined-out))

(define identity-f
  (d-function (empty-function-info)
              'identity
              (t-function (list i32) i32)
              (s-expr (e-let `(v) (list (llvm-val-param 0)) (list i32)
                             (s-return (e-ref 'v))
                             (e-void)))))
(define pow-f
  (d-function (empty-function-info)
              'pow
              (t-function (list i64 i64) i64)
              (s-expr (e-let `(x n) (list (llvm-val-param 0) (llvm-val-param 1)) (list i64 i64)
                             (s-if (e-llvm-icmp-ule (list (e-ref 'n) (ui64 0)))
                                   (s-return (ui64 1))
                                   (s-return (e-llvm-mul
                                              (list (e-ref 'x)
                                                    (e-op 'pow #f (list (e-ref 'x)
                                                                        (e-llvm-sub-nuw
                                                                         (list (e-ref 'n) (ui64 1)))))))))
                             (e-void)))))

(module+ test
  (require rackunit
           sham/ir/env
           sham/ir/dump
           sham/ir/verify)
  (define t-module
    (d-module (empty-module-info)
              'raw-sham-function-test-module
              (list identity-f pow-f)))
  (define s-env (build-sham-env t-module))
  (sham-dump-llvm-ir s-env)
  (sham-env-optimize! s-env #:opt-level 2)
  (sham-dump-llvm-ir s-env)
  (test-true "sham-raw-functions" (sham-verify-llvm-ir s-env)))
