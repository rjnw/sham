#lang racket

(require syntax/parse)
(provide (all-defined-out))

(define-splicing-syntax-class keyword-value
  (pattern (~seq k:keyword v:expr ...)
           #:attr spec (cons (string->symbol (keyword->string (syntax->datum #`k)))
                             (syntax->list #`(v ...)))))
(define-splicing-syntax-class keyword-info
  (pattern (~seq ki:keyword-value ...)
           #:attr spec (attribute ki.spec)))

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
  (let ([vs (assoc-default key lst dflt)]) (or vs dflt)))
(define (info-1value key lst (dflt #f))
  (match (info-value key lst dflt)
    [(list v) v]
    [v (or v dflt)]))
(define (add-info key val inf) (cons (list key val) inf))
(define (default-metadata . specs)
  (ormap (curry info-1value 'default-metadata) specs))
