#lang racket

(require sham)


(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))
  (current-sham-module test-module)

  (define-sham-function
    (const42 : i32)
    (ret (ui32 42)))
  (define-sham-function
    (identity (v : i32) : i32)
    (ret v))
  (define-sham-function
    (even? (x : i32) : i32)
    (if^ (icmp-eq (urem x (ui32 2)) (ui32 0))
         (ret (ui32 1))
         (ret (ui32 0))))
  (parameterize ([compile-options (list 'pretty 'dump)])
    (compile-sham-module!
     test-module
     #:opt-level 3))

  (check-eq? (sham-app even? 42) 1)
  (check-eq? (sham-app const42) 42)
  (check-eq? (sham-app identity 42) 42)
  )
