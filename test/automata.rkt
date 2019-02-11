#lang racket

(require rcf/ast
         (for-syntax syntax/parse racket/pretty))

;; (define-ast machine
;;   #:custom-write #t
;;   (def
;;     [dfa (start
;;           (end ...)
;;           [state ([evt next-state]
;;                   ...)]
;;           ...)]))

(define-syntax (dfa stx)
  (syntax-parse stx
    [(_ start (end ...) [state ([evt next-state] ...)] ...)
     (define fs (for/list  ([s (syntax->list #`(state ...))]
                            [sevt (syntax->list #`((evt ...) ...))]
                            [snxs (syntax->list #`((next-state ...) ...))])
                  (pretty-display s)
                  #`(define-sham-function #,s (input : i64) : )
                  (pretty-display sevt)
                  (pretty-display snxs)
                  42))
     #'42]))

(define M^
  (dfa s1 (s1)
       [s1 ([0 s2]
            [2 s1])]
       [s2 ([0 s1]
            [2 s2]
            [4 s2])]))

(define (s1 e)
  (match e
    [0 s2]
    [2 s1]))
(define (s2 e)
  (match e
    [0 s1]
    [2 s2]
    [4 s2]))
(define M (cons s1 (list s1)))
(define (machine-accepts? M ls)
  (define final
    (for/fold ([current (car M)])
              ([l ls])
      (current l)))
  (not (false? (member final (cdr M)))))
(machine-accepts? M (list 2 0 4 0 2))
