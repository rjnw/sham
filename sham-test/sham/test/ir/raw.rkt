#lang racket

(require sham/ir/ast
         sham/ir/builder
         sham/llvm/ir/md
         (prefix-in llvm- sham/llvm/ir/simple))

(define identity-f
  (d-function (empty-function-info)
              'identity
              (t-function (list i32) i32)
              (s-expr (e-let `(v) (list (llvm-val-param 0)) (list i32)
                             (s-return (e-ref 'v))
                             (e-void)))))

(module+ test
  (require rackunit
           sham/ir/env
           sham/ir/dump
           sham/ir/verify)
  (define t-module
    (d-module (empty-module-info)
              'raw-sham-function-test-module
              (list identity-f)))
  (define s-mod (build-sham-module t-module))
  (pretty-print (sham-module-ll-ast s-mod))
  (define s-env (build-sham-env t-module))
  (sham-dump-llvm-ir s-env)
  (test-true "sham-raw-functions" (sham-verify-llvm-ir s-env)))
