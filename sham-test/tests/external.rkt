#lang racket

(require sham)


(module+ test
  (require rackunit)

  (define test-module
    (create-empty-sham-module "test-module"))

  (current-sham-module test-module)

  (define-sham-function
    (test-atan (inp : f64) : f64)
    (ret (app^ (re #f 'atan f64)
               inp)))

  (define-sham-function
    (test-atan2 (inp1 : f64) (inp2 : f64) : f64)
    (ret (app^ (re 'libm 'atan2 f64)
               inp1 inp2)))

  (define-sham-function
    (test-printf (inp : i64) : i64)
    (ret (app^ (re #f 'printf i64 #t)
               (cstring "printf test %d\n") inp)))

  (parameterize ([compile-options (list 'pretty 'dump)])
    (compile-sham-module!
     test-module
     #:opt-level 3))
  (check-= (sham-app test-atan 10.0) (atan 10.0) 0.00001)
  (print (sham-app test-atan2 10.0 20.0))
  (sham-app test-printf 42))
