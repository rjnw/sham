#lang racket

(require racket/generic
         racket/syntax
         syntax/parse)
(require "ast-syntax-structs.rkt"
         "ast-syntax-class.rkt")
(provide (all-defined-out))

(define-generics ast-formatter
  (format-group-id ast-formatter group-spec)
  (format-group-parent ast-formatter group-spec)
  (format-group-args ast-formatter group-spec)
  (format-node-id ast-formatter group-spec node-spec)
  (format-node-args ast-formatter group-spec node-spec))

(struct rkt-struct-formatter
    [top-id spec group-seperator node-seperator]
    #:methods gen:ast-formatter
    [(define (format-group-id af gs)
       (match-define (rkt-struct-formatter id spec gsep nsep) af)
       (match-define (ast:group gname gparent gnodes ginfo) gs)
       (cond
         [(lookup-group-spec spec gparent)
          =>
          (λ (p)
            (format-id gname "~a~a~a" p gsep gname))]
         [else (format-id gname "~a~a~a" id gsep gname)]))
     (define (format-group-parent af gs)
       (match-define (rkt-struct-formatter id spec gsep nsep) af)
       (match-define (ast:group gname gparent gnodes ginfo) gs)
       (if gparent (format-group-id af (lookup-group-spec af gparent)) #f))
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
       (printf "format-node-args: ~a\n" (node-args ns))
       (node-args ns))])

(define-generics ast-builder
  (build-group-methods ast-builder group-spec)
  (build-group-generics ast-builder group-spec)
  (build-node-methods ast-builder group-spec node-spec)
  #:defaults ([any/c
               (define (build-group-methods b gs) #f)
               (define (build-group-generics b gs) #f)
               (define (build-node-methods b gs ns) #f)]))
