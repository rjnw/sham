#lang racket

(require "spec.rkt")

(provide (all-defined-out))

(define (group-arg-type-assoc gargs spec)
  (match-define (ast:group id parent nodes ginfo) spec)
  (for/list ([orig (info-value ginfo `#:common)]
             [fmted gargs])
    (cons orig
          (match (symbol->string (syntax->datum fmtd))
            [(regexp #rx"^!identifier$") (ast:type:identifier)]
            [(regexp #rx"^!(.*)$" (list _ checker)) (ast:type:unboxed checker)]
            [(pregexp #px"^(.*):(.*)" (list _ id type))
             (ast:type:internal
              (map (compose (curry datum->syntax #f) string->symbol)
                   (string-split type ".")) 0)]))))

(define (node-arg-type nspec)
  (match-define (ast:node id pat info) nspec)
  (define (rec pat depth)
    (match pat
      [(ast:pat:single s) (ast:node:arg s (type-from-syntax s depth) #f)]
      [(ast:pat:datum d) #f]
      [(ast:pat:checker c s) (ast:node:arg s (ast:type:external c depth) #f)]
      [(ast:pat:multiple s) (map (curryr rec depth) s)]
      [(ast:pat:repeat r) (rec r (add1 depth))]))
  (filter (compose not false?) (flatten (rec pat 0))))
