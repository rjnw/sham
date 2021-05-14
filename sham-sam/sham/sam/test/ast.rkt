#lang racket
(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))

(provide ast)

(define-ast ast
  (e
   [a '-]
   [b (a:e)]
   [c (a:e b:e.b)]
   [d (a:e (b:e ... c:e) ...)]
   [e (a:e (b:e ...))]
   [f (a:e (b:e ...) ...)]
   [g (a:e ...)]))

(module+ test
  (require rackunit)
  (define g1 (ast:e:d 'a1 ('b11 'b12 'b13 'c1) ('b21 'b22 'c2)))
  )
