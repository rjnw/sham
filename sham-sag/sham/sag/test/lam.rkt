#lang racket

(require sham/sag/ast
         sham/sag/custom)

(module+ test
  (define-ast LC
    (expr
     [lambda ('lambda (n) body)
       [body : expr]
       ;; #:identifier (n)
       ;; #:bind [n #:in-scope body]
       ]
     [letrec ('letrec ((ids vals) ...) e)
       [vals : expr]
       [e : expr]
       ;; #:identifier (ids)
       ]
     [app (rator rand)
          [rator : expr]
          [rand : expr]]
     [sym s
          [s : $identifier]])
    (terminal #:terminals
              [n number?])
    ;; #:prefix ||
    ;; #:top-seperator ||
    ;; #:seperator -
    #:with map-generic sexp-printer)

  (define lr (LC:expr:letrec '(a b c) '(1 2 3) 'd))
  (define parsed-letrec ($LC:expr (letrec ((a 1) (b 2) (c 3)) (+ a b c))))
  (printf "LC:")
  (pretty-print LC))
