#lang racket
(require "ast.rkt")

(provide (all-defined-out))

(struct ast:scope [kind instance])
(define default-ast-id-scope (ast:scope #f #f))
(define (new-id-scope kind) (ast:scope kind (gensym kind)))

(define ast:id/c (or/c symbol? syntax? ast:id?))

(struct ast:id:def ast:id [gen scope (maybe-refs #:mutable)])
(struct ast:id:ref ast:id [(maybe-def #:mutable)])

(define (ast-id-stxid aid)
  (cond [(ast:id? aid) (ast:id-stxid aid)]
        [(syntax? aid) aid]
        [(symbol? aid) (datum->syntax #f aid)]
        [else (error 'sham/sam/id "unknown identifier ~a" aid)]))
(define (add-id-ref! def ref)
  (when (ast:id:def? def)
    (set-ast:id:def-maybe-refs! def (cons ref (or (ast:id:def-maybe-refs def) '())))))

(define (id-create-ref def)
  (match-define (ast:id:def md stxid gen scope maybe-refs) def)
  (define ref (ast:id:ref md stxid def))
  (add-id-ref! def ref)
  ref)

(define (id-orig=? i1 i2)
  (equal? (ast-id-stxid i1) (ast-id-stxid i2)))

(define (id-def=? i1 i2)
  (and (ast:id:def? i1) (ast:id:def? i2) (id-orig=? i1 i2) (id-ref=? i1 i2)))
(define (id-ref=? i1 i2)
  (and (ast:id:ref? i1) (ast:id:ref? i2) (id-orig=? i1 i2) (id-ref=? i1 i2)))

(define (id=? i1 i2)
  (or (equal? i1 i2) (id-def=? i1 i2) (id-ref=? i1 i2)))
(define (any-id=? i1 i2)
  (or (id=? i1 i2)
      (and (and (ast:id? i1) (ast:id? i2))
           (or ;; (id-gen=? i1 i2)
               (id-orig=? i1 i2)))))

(define (make-scope-mapping assocs) (make-hasheqv assocs))
(define (make-id-mapping assocs) (make-hash assocs))

(define (find-def-for-id stxid scope maybe-scope-map)
  (and (hash? maybe-scope-map)
       (hash-ref (hash-ref maybe-scope-map scope) stxid)))
