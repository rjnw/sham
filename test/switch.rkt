#lang racket

(require sham
         sham/ast-utils
         sham/jit-utils)

(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))
  (current-sham-module test-module)

  (define-sham-function
    (foo (x : i64) : i64)
    (switch x
            (list (ui64 0) (ui64 1) (ui64 2))
            (list (ret (ui64 10)) (ret (ui64 20)) (ret (ui64 30)))
            (ret (ui64 0))))

  (parameterize ([compile-options (list 'pretty 'dump 'verify)])
    (compile-sham-module!
     test-module
     #:opt-level 0))
  (check-eq? (sham-app foo 1) 20)
  (check-eq? (sham-app foo 2) 30))
