#lang racket
(require "../main.rkt"
         "../private/ast-utils.rkt"
         "../private/jit-utils.rkt"
         "../private/parameters.rkt"
         "../private/info.rkt")

(current-sham-module (create-empty-sham-module "pow-module"))

(define-sham-function
  (pow (x : i64) (n : i64)) : i64
  (if^ (icmp-ule n (ui64 0))
       (return (ui64 1))
       (return (mul x (pow x (sub n (ui64 1)))))))

(define-sham-function
  (pow5 (x : i64)) : i64
  (return (pow x (ui64 5))))

(parameterize ([compile-options (list 'pretty 'dump)])
  (compile-sham-module! (current-sham-module) #:opt-level 0))

(sham-app pow 2 10)
(sham-app pow5 2)
