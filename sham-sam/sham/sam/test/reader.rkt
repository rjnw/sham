#lang racket

(require sham/sam/reader
         sham/sam/ast
         ;; "ast.rkt"
         )

(define-ast ast
  (e
   [a '-]
   [b (a:e)]
   ;; [c (a:e b:e.b)]
   ;; [d (a:e (b:e ... c:e) ...)]
   ;; [e (a:e (b:e ...))]
   ;; [f (a:e (b:e ...) ...)]
   [g (a:e ...)]))

(build-reader ast)
