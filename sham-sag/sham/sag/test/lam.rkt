#lang racket

(require sham/sag/ast
         sham/sag/custom
         sham/sag/runtime
         (for-syntax sham/sag/spec))

(define-ast LC
  (expr
   [lambda ('lambda (n) body)
     #:type ([body : expr])
     ;; #:identifier (n)
     ;; #:bind [n #:in-scope body]
     ]
   [letrec ('letrec ((ids vals) ...) e)
     #:type
     ([vals : expr]
      [e : expr])
     ;; #:identifier (ids)
     ]
   [app (rator rand ...)
        #:type
        ([rator : expr]
         [rand : expr])]
   [sym !identifier]
   [num !integer])
  ;; #:prefix ||
  ;; #:top-seperator ||
  ;; #:seperator -
  #:with map-generic sexp-printer)

(module+ test
  (begin-for-syntax
    (require racket/pretty)
    (require racket)
    (printf "syntax-value:LC\n")
    (define-values (lcv _) (syntax-local-value/immediate #`LC))
    (pretty-print (list (ast-id lcv) (ast-sid lcv) (ast-groups lcv) (ast-info lcv))))

  ;; (define lr (LC:expr:letrec '(a b c) '(1 2 3) 'd))
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
