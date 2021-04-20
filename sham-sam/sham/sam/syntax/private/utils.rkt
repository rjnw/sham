#lang racket

(provide (all-defined-out))

;;; keyword info is stored in (list (cons keyword values) ...)
;;  combine duplicate key value pair into one pair
;;  ... (a . v0) ... (a . v1) ... => ... (a v0 v1) ...
(define (dedup-assoc i)
  (define (combine ls) (cons (caar ls) (map cdr ls)))
  (map combine (group-by car i)))

(define (assoc-default key lst (dflt #f) (is-equal? equal?))
  (let ([v (assoc key lst is-equal?)])
    (if v (cdr v) dflt)))

(define info/c (listof (cons/c symbol? (listof syntax?))))

(define (info-value key lst (dflt #f))
  (let ([vs (assoc-default key lst dflt)]) (if (cons? vs) (car vs) (or vs dflt))))

(define (find-first lst f?)
  (if (empty? lst)
      lst
      (or (and (f? (car lst)) (car lst))
          (find-first (cdr lst) f?))))

;; counts `c?` items in a sequence until `stop?`
;;  both c? and stop? take value and index
(define (count-until seq c? stop?)
  (for/fold ([c 0])
            ([v seq]
             [i (sequence-fold (λ (i v) (+ i 1)) 0 seq)]
             #:break (stop? v i))
    (+ c (if (c? v i) 1 0))))

;; contracts
(define (maybe/c x/c) (or/c false/c x/c))
(define (assoc/c key/c value/c) (listof (cons/c key/c value/c)))

;; syntax utils
(define syntax->string (compose symbol->string syntax->datum))
(define (string->syntax str (ctxt #f)) (datum->syntax ctxt (string->symbol str)))

(define (->symbol s)
  (cond [(syntax? s) (syntax->datum s)]
        [(string? s) (string->symbol s)]
        [(or (integer? s) (symbol? s)) s]
        [else (error 'sham/sam "->symbol: couldn't force ~a to symbol" s)]))

(define (ooo? s)
  (let ([se (syntax-e s)])
    (and (symbol? se)
         (regexp-match #px"^(([[:digit:]]*)(_|\\.)+([[:digit:]]*))$" (symbol->string se)))))

;; ooo: syntax? -> (maybe/c (cons/c (maybe/c postive-number?) (maybe/c positive-number?)))
;;  returns: (cons min max) for a range specified in given syntax
;;  corresponds to `ooo` in racket's match grammar but with additional options
;;  matches for k, ..., ___, ..k, __k, k.., k__, k..k, k__k
(define (ooo p)
  (match (ooo? p)
    [#f #f]
    [(list a b mn o mx) (cons (string->number mn) (string->number mx))]))

(module+ test
  (require rackunit)
  (check-equal? (ooo #'a) #f)
  (check-equal? (ooo #'(a b)) #f)
  (check-equal? (ooo #'(... ...)) (cons #f #f))
  (check-equal? (ooo #'..42) (cons #f 42))
  (check-equal? (ooo #'42..) (cons 42 #f))
  (check-equal? (ooo #'4..2) (cons 4 2)))
