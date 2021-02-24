#lang racket

(require syntax/parse)
(require "spec.rkt"
         "syntax-class.rkt")

(provide (all-defined-out))

(define (info->hash i)
  (define (combine ls)              ;; ((1 . a) (1 . b)) => (1 a b)
    (cons (caar ls)
          (map cdr ls)))
  (make-hash (map combine (group-by car i))))

(define (info-values infos key)
  (cond [(list? infos) (map cdr
                            (filter (λ (kvp) (equal? (car kvp) key))
                                    infos))]
        [(hash? infos) (hash-ref infos key '())]))

(define (info-value infos key (dflt #f))
  (define vs (info-values infos key))
  (cond
    [(cons? vs) (car vs)]
    [(empty? vs) dflt]))

(define (map-pat pat f-single f-datum f-multiple f-repeat)
    (define (rec pat)
      (match pat
        [(ast:pat:single s) (f-single s)]
        [(ast:pat:datum d) (f-datum d)]
        [(ast:pat:checker c s) (f-single s)]
        [(ast:pat:multiple s) (f-multiple (map rec s))]
        [(ast:pat:repeat r k) (f-repeat (rec r))]))
    (rec pat))

(define (lookup-group-spec spec gsyn)
  (define gdat
    (cond
      [(symbol? gsyn) gsyn]
      [(syntax? gsyn) (syntax->datum gsyn)]
      [else #f]))
  (cond
    [(and gdat (hash? (ast-groups spec)))
     (hash-ref (ast-groups spec) gdat #f)]
    [(and gdat (list? (ast-groups spec)))
     (findf (λ (g) (equal? (syntax->datum (ast:group-id g))
                           gdat))
            (ast-groups spec))]
    [else #f]))

(define (group-args as gs (kw `#:common))
  (if gs
      (append (map (λ (s)
                     (syntax-parse s
                       [i:identifier (cons #`i #f)]
                       [(i:identifier ki:keyword-info) (cons #`i (attribute ki.spec))]))
                   (info-values (ast:group-info as) kw))
              (group-args as (lookup-group-spec as (ast:group-parent gs)) kw))
      `()))

;; -> (maybe/c (list/c syntax))
(define (node-args node-spec)
  (flatten (map-pat (ast:node-pattern node-spec) identity (const '()) append identity)))

(define (split-identifier syn)
  (define back-to-syn (compose (curry datum->syntax syn) string->symbol))
  (match (symbol->string (syntax->datum syn))
    [(regexp #rx"^!(.*)$" (list _ checker)) (list (back-to-syn checker) '!)]
    [s
     (match (string-split s ":")
       [(list id type)
        (map back-to-syn
             (cons id (string-split type ".")))]
       [(list id) (list (back-to-syn id))])]))

(define id-without-type (compose car split-identifier))
(define type-from-id (compose cdr split-identifier))
