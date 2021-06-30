#lang racket

(require sham/sam/compiler
         sham/sam/rkt)
(require "../ast/arith.rkt")

(struct op [rator rands])

(define-compiler (interpret-arith)
  (math -> rkt)
  (iexpr (expr -> any)
         [(neg (^ e)) (op - (list e))]
         [(div (^ n) (^ d)) (op / (list n d))]
         [(add (^ e) ...) (op + e)]
         [(mul (^ e) ...) (op * e)]))
