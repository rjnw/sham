#lang racket

(require sham/sag/ast
         sham/sag/custom
         sham/sag/runtime
         (for-syntax sham/sag/syntax/spec))

(define-ast math
  (expr
   [neg ('- e)]
   [div ('/ n d)]
   [plus ('+ e ...)]
   [minus ('- e1 e2 ...)]
   [mult ('* e ...)])
  #:with struct-helpers
  )

(module+ test
  (begin-for-syntax
    (require racket/pretty
             racket)
    (require sham/sag/syntax/runtime)
    (define-values (mcv _) (syntax-local-value/immediate #`math))
    ;; (pretty-print mcv)
    ;; (pretty-print (pretty-spec mcv))
    )
  (require rackunit)
  (define mdiv1 (make-div 4 2))
  (check-equal? (div-n mdiv1) 4)
  )
