#lang racket

(require sham/sam/transform
         sham/sam/rkt)
(require "../ast/basic-math.rkt")

(define-transform (interpret-arith)
  (math-ast -> rkt)
  (iexpr (val -> any)
         [(num n) n]
         [(neg (^ e)) (- e)]
         [(div (^ n) (^ d)) (/ n d)]
         [(add (^ es) ...) (apply + es)]
         [(mul (^ es) ...) (apply * es)]
         ))

(module+ test
  (require rackunit)
  (define m1 (neg (num 42)))
  (define m2 (div m1 (num 2)))
  (check-equal? (interpret-arith m1) -42)
  (check-equal? (interpret-arith m2) -21)
  (check-equal? (interpret-arith (mul (num 21) (num 2))) 42)
  )
