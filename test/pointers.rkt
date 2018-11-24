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
    (create-box) : i32*
    (ret (malloc^ (etype i32))))
  (define-sham-function
    (store-box (x : i32) (box : i32*)) : tvoid
    (store! x box)
    ret-void)
  (define-sham-function
    (open-box (box : i32*)) : i32
    (ret (load box)))
  (define-sham-function
    (free-box (x : i32*)) : tvoid
    (free^ x)
    ret-void)

  (parameterize ([compile-options (list 'pretty 'dump 'mc-jit)])
    (compile-sham-module!
     test-module
     #:opt-level 3))

  (define tbox (sham-app create-box))
  (sham-app store-box 42 tbox)
  (check-eq?  (sham-app open-box tbox) 42)
  (sham-app free-box tbox))
