#lang racket

(require racket/generic
         racket/syntax
         syntax/parse)
(require "spec.rkt"
         "syntax-class.rkt"
         "utils.rkt")
(provide (all-defined-out))

(define-generics id-formatter
  (format-group-id id-formatter group-spec)
  (format-group-args id-formatter group-spec)
  (format-node-id id-formatter group-spec node-spec)
  (format-node-args id-formatter group-spec node-spec))

(struct rkt-struct-formatter
    [top-id spec group-seperator node-seperator]
    #:methods gen:id-formatter
  [(define (format-group-id af gs)
     (match-define (rkt-struct-formatter id spec gsep nsep) af)
     (match-define (ast:group gname gparent gnodes ginfo) gs)
     (cond
       [(lookup-group-spec spec gparent)
        =>
        (λ (p)
          (format-id gname "~a~a~a" p gsep gname))]
       [else (format-id gname "~a~a~a" id gsep gname)]))
   (define (format-group-args af gs)
     (match-define (rkt-struct-formatter id spec gsep nsep) af)
     (match-define (ast:group gname gparent gnodes ginfo) gs)
     (define (format-arg c)
       (syntax-parse c
         [i:identifier #`i]
         [(i:identifier ki:keyword-info)
          (define if-default (info-value (attribute ki.spec) `#:default))
          (define if-mutable (info-value (attribute ki.spec) `#:mutable))
          #`(i #,@(if if-default (list #`#:auto) `())
               #,@(if if-mutable (list #`#:mutable) `()))]))
     (map format-arg (info-values ginfo `#:common)))
   (define (format-node-id af gs ns)
     (match-define (rkt-struct-formatter id spec gsep nsep) af)
     (format-id (ast:node-id ns) "~a~a~a" (format-group-id af gs) nsep (ast:node-id ns)))
   (define (format-node-args af gs ns)
     (match (ast:node-pattern ns)
       [(ast:pat:single t pid)
        (list
         (match (symbol->string (syntax->datum pid))
           [(pregexp "!(.*)" (list orig without)) (format-id pid "~a" without)]
           [else pid]))]
       [else (node-args ns)]))])
