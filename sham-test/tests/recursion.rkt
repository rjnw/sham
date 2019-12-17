#lang racket

(require sham)

(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))
  (current-sham-module test-module)

  (define-sham-function
    (meven? (x : i32) : i1)
    (if^ (icmp-eq (urem x (ui32 2)) (ui32 0))
         (ret (ui1 1))
         (ret (modd? (sub x (ui32 1))))))
  (define-sham-function
    (modd? (x : i32) : i1)
    (if^ (icmp-eq (urem x (ui32 2)) (ui32 0))
         (ret (ui1 0))
         (ret (meven? (sub x (ui32 1))))))

  (parameterize ([compile-options (list 'pretty 'dump)])
    (compile-sham-module!
     test-module
     #:opt-level 3))
  (check-eq? (sham-app meven? 42) 1)
  (check-eq? (sham-app modd? 42) 0))
