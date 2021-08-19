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

(struct op [rator rands])

(define-compiler (interpret-arith)
  (math -> rkt)
  (iexpr (expr -> any)
         [(neg (^ e)) (op - (list e))]
         ;; [(div (^ n) (^ d)) (op / (list n d))]
         ;; [(add (^ es) ...) (op + es)]
         ;; [(mul (^ es) ...) (op * es)]
         ))
