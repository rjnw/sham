#lang racket

(require (submod "spec.rkt" ast)
         (submod "generics.rkt" ast)
         "generics.rkt"
         "format.rkt"
         "kw-info.rkt")
(provide (all-defined-out))

(define (build-syntax raw-ast-spec)
  (define formatter (ast-format (info-1value `format (ast-info raw-ast-spec))))
  (define default-builders
    (cond [(info-value `default (ast-info raw-ast-spec)) => (λ (f) ((syntax-local-value f)))]
          [else (default-rkt-struct-builder)]))
  (define raw-builders
    (append (flatten (map (λ (g) ((syntax-local-value g))) (assoc-default `with (ast-info raw-ast-spec) '())))
            default-builders))

  (define all-builders (foldr update-others raw-builders raw-builders))
  (define (foldr-builders f base) (foldr f base all-builders))

  (define ast-spec (foldr-builders update-spec (build-ast-spec raw-ast-spec formatter)))

  (define top-struct (foldr-builders (curryr build-top ast-spec) empty))
  (define (group-def group-spec)
    (define (node-def node-spec) (foldr-builders (curryr build-node ast-spec group-spec node-spec) empty))
    (define node-defs (append-map node-def (group-nodes group-spec)))
    (define struct-defs (foldr-builders (curryr build-group ast-spec group-spec) empty))
    (append struct-defs node-defs))

  (values
   (map ->syntax
        (append top-struct
                (append-map (compose group-def cdr) (ast-groups ast-spec))))
   ast-spec))
