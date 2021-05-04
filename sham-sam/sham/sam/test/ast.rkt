#lang racket
(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))


(define-ast ast
  (e
   [a '-]
   [b (a:e)]
   [c (a:e a:e.b)]
   [d (a:e ...)]
   [e (a:e (b:e ...))]
   [f (a:e (b:e ...) ...)]
   [g ((a:e ...) (b:e ... c:e) ...)]))

(module+ test
  (require rackunit)
  (define g1 (ast:e:g ('a1 'a2 'a3) ('b11 'b12 'b13 'c1) ('b21 'b22 'c2)))
  )
