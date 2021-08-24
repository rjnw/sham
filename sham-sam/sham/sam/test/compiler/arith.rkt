#lang racket

(require sham/sam/compiler
         sham/sam/rkt)
(require "../ast/arith.rkt")
#;(define-ast math
  (expr
   [neg ('- e)]
   [div ('/ n d)]
   [add ('+ e ...)]
   [sub ('- e1 e2 ...)]
   [mul ('* e ...)])
  #:with struct-helpers sexp-printer
  #:format (#f - #f - -))

(define-compiler (interpret-arith)
  (math -> rkt)
  (iexpr (expr -> any)
         [(and n (? number?)) n]
         [(neg (^ e)) (- e)]
         [(div (^ n) (^ d)) (/ n d)]
         ;; [(add (^ es) ...) (op + es)]
         ;; [(mul (^ es) ...) (op * es)]
         ))

(module+ test
  (require rackunit)
  (define m1 (neg 42))
  (define m2 (div (neg 42) 2))
  (check-equal? (interpret-arith m1) -42)
  (check-equal? (interpret-arith m2) -21))
