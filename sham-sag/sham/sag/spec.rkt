#lang racket

(provide (all-defined-out))

(struct ast (id groups info) #:prefab)
(struct ast:group (id syn-id parent common-args nodes info) #:prefab)
(struct ast:node (id flat-args pattern info) #:prefab)

(define (lookup-group-spec group-id ast-spec)
  (cond [(syntax? group-id) (hash-ref (pub:ast-groups ast-spec) (syntax->datum group-id))]
        [(symbol? group-id) (hash-ref (pub:ast-groups ast-spec) group-id)]
        [else #f]))
