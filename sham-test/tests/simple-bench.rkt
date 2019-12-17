#lang racket

(require sham)



(require rackunit)
(define test-module
  (create-empty-sham-module "test-module"))
(current-sham-module test-module)

(define-sham-function
  (const42) :  i32
  (ret (ui32 42)))

(parameterize ([compile-options (list 'pretty 'dump)])
  (compile-sham-module!
   test-module
   #:opt-level 3))
(time (sham-app const42))
