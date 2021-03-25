#lang racket

(require sham/sag/ast
         sham/sag/custom
         sham/sag/runtime
         (for-syntax sham/sag/syntax/spec))

(define-ast LC
  (expr
   [lambda ('lambda (n) body)]
   [letrec ('letrec ((ids vals) ...) e)]
   [app (rator rand ...)]
   [sym !identifier]
   [num !integer])
  #:with struct-helpers)

(module+ test
  (begin-for-syntax
    (require racket/pretty)
    (require racket)
    (printf "syntax-value:LC\n")
    (define-values (lcv _) (syntax-local-value/immediate #`LC))
    (pretty-print (pretty-spec lcv))
    ;; (pretty-print (list (ast-id lcv) (ast-syn-id lcv) (ast-groups lcv) (ast-info lcv)))
    )

  (require rackunit)
  (define lr1 (make-letrec `(a b c) `(1 2 3) `d))
  (define lr2 (LC:expr:letrec (['a 1] ['b 2] ['c 3]) 'd))
  (check-equal? (letrec-ids lr2) `(a b c))
  (check-equal? (letrec-vals lr2) `(1 2 3))
  (check-equal? (letrec-e lr2) 'd)

  (define lam1 (make-lambda 'a 'b))
  (define lam2 (LC:expr:lambda ('a) 'b))
  (check-equal? (lambda-n lam2) 'a)
  (check-equal? (lambda-body lam2) 'b)
  ;; (match lr2 [(LC:expr:letrec ([i1 v1] [i2 v2] [i3 v2]) e) (list i v e)])
  ;; (define-transformation
  ;;   constant-fold (LC:expr -> LC:expr)
  ;;   [(app (sym +) (num a) (num a))
  ;;    (num (+ a b))])

  #;(define-reduction
      total-letrec LC:expr
      0 +
      [(letrec _ e)
       (+ 1 (total-letrec e))])
  )
