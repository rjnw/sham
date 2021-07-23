#lang racket
(require syntax/parse)
(provide (all-defined-out))

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

(struct stx:ooo [s cnt] #:prefab)

(define-syntax-class single-pattern
  (pattern i:id #:attr result #`i)
  (pattern (m:multiple-pattern ...) #:attr result (attribute m.result)))
(define-splicing-syntax-class multiple-pattern
  (pattern (~seq maybe-repeat:expr maybe-ooo:id)
           #:when (ooo? #`maybe-ooo)
           #:attr result (stx:ooo (attribute maybe-repeat) (ooo #`maybe-ooo)))
  (pattern s:expr #:attr result (attribute s)))

;; parses a syntax at one level to either identifier / (listof (or syntax stx:ooo?))
;;  basically syntax->datum but with stx:ooo spliced if there
(define (parse-for-ooo stx)
  (syntax-parse stx
    [e:single-pattern (attribute e.result)]))

(module+ test
  (require rackunit)

  (check-equal? (ooo #'a) #f)
  (check-equal? (ooo #'(a b)) #f)
  (check-equal? (ooo #'(... ...)) (cons #f #f))
  (check-equal? (ooo #'..42) (cons #f 42))
  (check-equal? (ooo #'42..) (cons 42 #f))
  (check-equal? (ooo #'4..2) (cons 4 2)))
