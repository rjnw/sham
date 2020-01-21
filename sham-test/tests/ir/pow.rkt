#lang racket

(require sham)

(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))
  (current-sham-module test-module)

  (define-sham-function
    (pow (x : i64) (n : i64) : i64)
    (if^ (icmp-ule n (ui64 0))
         (return (ui64 1))
         (return (mul x (pow x (sub-nuw n (ui64 1)))))))

  (parameterize ([build-options (list 'pretty 'verify 'dump)])
    (build-sham-ir! (current-sham-module)))
  (sham-dump-llvm (current-sham-module)))
