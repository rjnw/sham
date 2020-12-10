#lang racket

(require syntax/parse)
(require "spec.rkt"
         "syntax-class.rkt")

(provide (all-defined-out))

(define (info-values infos key)
  (map syntax->list
       (map cdr
            (filter (λ (kvp) (equal? (syntax->datum (car kvp)) key))
                    infos))))
(define (info-value infos key)
  (define vs (info-values infos key))
  (cond
    [(cons? vs) (car vs)]
    [(empty? vs) #f]))

(define (map-pat pat f-single f-datum f-multiple f-repeat)
    (define (rec pat)
      (match pat
        [(ast:pat:single type s) (f-single s)]
        [(ast:pat:datum d) (f-datum d)]
        [(ast:pat:checker c s) (f-single s)]
        [(ast:pat:multiple s) (f-multiple (map rec s))]
        [(ast:pat:repeat r) (f-repeat (rec r))]))
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

(define (spec->storage top-id ast-spec)
  (define (ki i)
    #`(list #,@(map (λ (kv) #`(cons `#,(car kv) (list #,@(map (lambda (v) #`#'#,v) (syntax->list (cdr kv)))))) i)))
  (define (storage spec)
    (match spec
      [(ast groups info)
       #`(ast #,(if (hash? groups)
                    #`(make-hash (list
                                  #,@(for/list ([(gn gs) groups])
                                       #`(cons #'#,gn #,(storage gs)))))
                    #`(list #,@(map storage groups)))
              #,(ki info))]
      [(ast:group id parent nodes info)
       #`(ast:group #'#,id #,(if parent #'#,parent #`#f) (list #,@(map storage nodes)) #,(ki info))]
      [(ast:node id pat info)
       #`(ast:node #'#,id #,(storage pat) #,(ki info))]
      [(ast:pat:single type id)
       #`(ast:pat:single #'#,type #'#,id)]
      [(ast:pat:datum syn)
       #`(ast:pat:datum `#,syn)]
      [(ast:pat:multiple specs)
       #`(ast:pat:multiple (list #,@(map storage specs)))]
      [(ast:pat:repeat spec)
       #`(ast:pat:repeat #,(storage spec))]
      [(ast:pat:checker check id)
       #`(ast:pat:checker #'#,check #'#,id)]))
  (storage ast-spec))
