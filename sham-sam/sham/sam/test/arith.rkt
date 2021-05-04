#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))

(define-ast math
  (expr
   [neg ('- e)]
   [div ('/ n d)]
   [add ('+ e ...)]
   [sub ('- e1 e2 ...)]
   [mul ('* e ...)])
  #:with struct-helpers sexp-printer
  #:format (#f - #f - -))

(module+ test
  (begin-for-syntax
    (require racket/pretty
             racket)
    (require sham/sam/syntax/runtime)
    (define-values (mcv _) (syntax-local-value/immediate #`math))
    ;; (pretty-print mcv)
    ;; (pretty-print (pretty-spec mcv))
    )
  (require sham/sam/runtime/generics)
  ;; (- (- x)) -> x

  (require rackunit)
  (define mdiv1 (make-div 4 2))
  (check-equal? (div-n mdiv1) 4)
  (check-equal?
   (match (make-neg (make-neg 2))
     [(neg (neg x)) x])
   2)
  (define an2 (add (neg (neg 42))))
  (check-equal? (neg-e (neg-e (car (add-e an2)))) 42)

  ;; (define (fold-neg e)
  ;;   ((gfold-rec (λ (v) (cond [(vector? v) vector] [(list? v) list] [else #f]))
  ;;               (lambda (e) (print e) (match e
  ;;                              [(neg (neg x)) x]
  ;;                              [else e])))
  ;;              e))
  )
