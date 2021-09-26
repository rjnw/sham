#lang racket

(require sham/sam/transform
         sham/sam/rkt)
(require "../ast/basic-math.rkt")

#;(define-ast math
  (expr
   [neg ('- e)]
   [div ('/ n d)]
   [add ('+ e ...)]
   [sub ('- e1 e2 ...)]
   [mul ('* e ...)])
  #:with struct-helpers sexp-printer
  #:format (#f - #f - -))

(define-transform (interpret-arith)
  (math-ast -> rkt)
  (iexpr (val -> any)
         ;; [num num]
         [(neg (^ e)) (- e)]
         ;; [(div (^ n) (^ d)) (/ n d)]
         ;; [(add (^ es) ...) (apply + es)]
         ;; [(mul (^ es) ...) (apply * es)]
         ))

(module+ test
  (require rackunit)
  (define m1 (neg 42))
  (define m2 (div (neg 42) 2))
  (check-equal? (interpret-arith m1) -42)
  (check-equal? (interpret-arith m2) -21)
  (check-equal? (interpret-arith (mul 21 2)) 42))
