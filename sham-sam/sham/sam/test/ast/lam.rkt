#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))

(define-ast LC
  (expr
   [lambda ((args:id ...) body:expr)]
   [letrec ([(ids:id vals:expr) ...] e:expr)]
   [app (rator:expr rand:expr ...)]
   [sym s:identifier]
   [num n:integer])
  #:format (#f - #f - -))

(module+ test
  (begin-for-syntax
    (require racket racket/pretty)
    (require (submod sham/sam/syntax/spec ast))

    (define-values (lcv _) (syntax-local-value/immediate #`LC))
    (pretty-print (pretty-spec lcv))
    ;; (pretty-print (list (ast-id lcv) (ast-syn-id lcv) (ast-groups lcv) (ast-info lcv)))
    )

  (define lr (letrec [('a 1) ('b 2)] 'c))
  ;; (define parsed-letrec ($LC:expr (letrec ((a 1) (b 2) (c 3)) (app (sym +) a b c))))
  ;; (define parsed-expr ($LC:expr (letrec ((a 1) (b 2) (c 3)) (app (sym +) 1 2 3))))
  ;; (printf "LC:")
  ;; (pretty-print parsed-letrec)

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
