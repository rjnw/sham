#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))

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
    (define-values (lcv _) (syntax-local-value/immediate #`LC))
    ;; (pretty-print (pretty-spec lcv))
    ;; (pretty-print (list (ast-id lcv) (ast-syn-id lcv) (ast-groups lcv) (ast-info lcv)))
    )

  (require rackunit)
  (define lr1 (make-LC:expr:letrec `(a b c) `(1 2 3) `d))
  (define lr2 (LC:expr:letrec (['a 1] ['b 2] ['c 3]) 'd))
  (check-equal? (LC:expr:letrec-ids lr2) `(a b c))
  (check-equal? (LC:expr:letrec-vals lr2) `(1 2 3))
  (check-equal? (LC:expr:letrec-e lr2) 'd)

  (define lam1 (make-LC:expr:lambda 'a 'b))
  (define lam2 (LC:expr:lambda ('a) 'b))
  (check-equal? (LC:expr:lambda-n lam2) 'a)
  (check-equal? (LC:expr:lambda-body lam2) 'b)
  (check-equal?
   (match lr2 [(LC:expr:letrec ([i v] ...) e) (list i v e)])
   `((a b c) (1 2 3) d)))
