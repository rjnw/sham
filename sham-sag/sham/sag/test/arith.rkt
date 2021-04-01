#lang racket

(require sham/sag/ast
         sham/sag/custom
         sham/sag/runtime
         (for-syntax sham/sag/syntax/spec))

(define-ast math
  (expr
   [neg ('- e)]
   [div ('/ n d)]
   [add ('+ e ...)]
   [sub ('- e1 e2 ...)]
   [mul ('* e ...)])
  #:with struct-helpers)

(module+ test
  (begin-for-syntax
    (require racket/pretty
             racket)
    (require sham/sag/syntax/runtime)
    (define-values (mcv _) (syntax-local-value/immediate #`math))
    ;; (pretty-print mcv)
    ;; (pretty-print (pretty-spec mcv))
    )
  (require sham/sag/runtime/generics)
  ;; (- (- x)) -> x

  ;; (define (fold-neg e)
  ;;   (gmap #f (lambda (e) (match e [(math:expr:neg (math:expr:neg x)) x] [else e])) e))
  (require rackunit)
  (define mdiv1 (make-div 4 2))
  (check-equal? (div-n mdiv1) 4)
  (check-equal?
   (match (make-neg (make-neg 2))
     [(math:expr:neg (math:expr:neg x)) x])
   2)
  )
