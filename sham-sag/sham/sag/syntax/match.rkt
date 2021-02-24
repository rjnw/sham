#lang racket

(require "spec.rkt"
         (prefix-in rt: "runtime.rkt"))

(provide term-match-expander)

(define (ooo p)
  ;; corresponds to `ooo` in racket's match grammar
  ;; matches for ..., ___, ..k, __k
  ;; syntax? ->  (or/c false? (list/c (or/c false? integer?)))
  (define (sooo s)
    (match (regexp-match #px"^((__|\\.\\.)(_|\\.|[[:digit:]]+))$" s)
      [#f #f]
      [(list a b c k) (list (string->number k))]))
  (and (identifier? p)
       (sooo (symbol->string (syntax->datum p)))))


(define (remove-datum pt) (filter (not/c ast:pat:datum?) pt))
(define (kid s c) s)                  ;; todo check c is fully consumed
(define (consume c (n 1))
  (printf "consuming: ~a-~a\n" c n)
  (match c
    [(cons h t) (cons (if h (- h n) h) t)]))
(define (is-consumed? c)
  (match c
    [(list h t) (and (is-consumed? h) (is-consumed? t))]
    [null #t]
    [#f #t]
    [0 #t]
    [else #f]))

;; k : syntax? number? -> syntax?
;;   continuation with final match syntax and leftover repeat value
(define ((do-head pat stx) k c)
  (printf "do-head: \n\tpat: ~a, \n\tstx: ~a, \n\tc: ~a\n" (pretty-pattern pat) (syntax->datum stx) c)
  (match (ooo stx)
    [(list n) (k pat (consume (list n)))]
    [#f
     (match pat
       [(ast:pat:single id) (k stx (consume c))]
       [(ast:pat:datum syn) (k #f c)]
       [(ast:pat:multiple pms)
        (match* (pms (syntax-e stx))
          [((cons psh pst) (cons sth stt))
           ((do-head psh sth)
            (λ (s n) (k #`(vector . #,(filter (not/c false?) (cons s ((do-tail pst stt) psh n))))
                        (consume c)))
            `(1))])]
       [(ast:pat:repeat pr nr)
        (match (syntax-e stx)
          [(cons sth stt)
           (k ((do-head pr sth)
               (λ (s n) #`(list #,s .
                                #,((do-tail #f stt) pr n)))
               nr)
              (consume c))])]
       [(ast:pat:checker check id) (k stx (consume c))])]))

;; pat : (or/c false? ast:pat?)
;;;  #f for pat signifies tail for a repeat pattern and to match against prev instead
(define ((do-tail pat stx) prev c)
  (printf "do-tail: \n\tpat: ~a, \n\tstx: ~a, \n\tc: ~a\n"
          (if pat (map pretty-pattern pat) pat)
          (map syntax->datum stx) c)
  (match* (pat stx)
    [(null null) null]
    [((cons psh pst) (cons sth stt))
     (match (ooo sth)
       [(list n) (error "sham-sag: repeating match pattern for a non-repeating ast pattern")]
       [#f ((do-head psh sth)
            (λ (s n) (cons s ((do-tail pst stt) psh n)))
            c)])]
    [(#f null)
     (unless (is-consumed? c) (error "sham-sag: repeating pattern did not consume required number of terms"))
     null]
    [(#f (cons sth stt))
     (match (ooo sth)
       [(list n) ((do-tail pat stt) prev (consume c n))]
       [#f ((do-head prev sth)
            (λ (s n) (cons s ((do-tail pat stt) prev n)))
            c)])]))

(define (pattern-expander stx pat)
  ((do-head pat stx) kid `(1)))

(define (term-match-expander stx tt)
  (match-define (rt:term-type rt mt ss ts) tt)
  (match ss
    [(ast:node idt fid nargs pat ninfo)
     (pattern-expander stx pat)]
    [(ast:group idt fid prnt gargs nodes ginfo)
     (error 'sham:sag "todo match expander for group types: ~a" (car idt))]))

(module+ test
  (require rackunit)
  (define pt
    (ast:pat:multiple
     (list
      (ast:pat:datum `letrec)
      (ast:pat:multiple
       (list
        (ast:pat:repeat
         (ast:pat:multiple
          (list (ast:pat:single #'ids) (ast:pat:single #'vals)))
         (list #f))))
      (ast:pat:single #'e))))
  (pretty-print (pattern-expander #`(`letrec (([a b] (... ...))) e) pt)))
