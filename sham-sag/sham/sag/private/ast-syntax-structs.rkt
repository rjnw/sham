#lang racket

(provide (all-defined-out))

(struct ast (groups info) #:prefab)
(struct ast:group (id parent nodes info) #:prefab)
(struct ast:node (id) #:prefab)
(struct ast:node:pat ast:node (pattern info) #:prefab)
(struct ast:node:term ast:node (proc) #:prefab)
(struct ast:node-contract [spec] #:prefab)

(struct ast:pat:single   (type id) #:prefab)
(struct ast:pat:datum    (syn) #:prefab)
(struct ast:pat:multiple (specs) #:prefab)
(struct ast:pat:repeat   (spec) #:prefab)
(struct ast:pat:checker  (check id) #:prefab)
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

;; -> (maybe/c (list/c syntax))
(define (node-args node-spec)
  (match node-spec
    [(ast:node:pat id pat info)
     (flatten (map-pat pat identity (const '()) append identity))]
    [(ast:node:term id proc) #f]))
