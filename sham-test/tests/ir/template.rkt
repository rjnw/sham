#lang racket

(require sham/ast
         sham/ir)

(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))
  (current-sham-module test-module)

  (define-sham-function
    (<id> (<arg> : <arg-type>) ...) : <return-type>
    <body>)

  (parameterize ([build-options (list 'pretty 'verify 'dump)])
    (build-sham-ir! (current-sham-module)))
  (sham-dump-llvm (current-sham-module)))
