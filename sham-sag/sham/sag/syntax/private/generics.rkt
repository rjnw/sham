#lang racket

(require racket/generic
         racket/syntax
         syntax/parse)
(require "spec.rkt"
         "syntax-class.rkt"
         "utils.rkt")
(provide (all-defined-out))

(define-generics id-formatter
  (format-group-id id-formatter id top-spec group-spec)
  (format-group-arg-id id-formatter id top-spec group-spec)
  (format-node-id id-formatter id top-spec group-spec node-spec)
  (format-node-arg-id id-formatter id top-spec group-spec node-spec))

(struct basic-id-formatter
    [top-id group-seperator node-seperator]
    #:methods gen:id-formatter
  [(define (format-group-id af id ts gs)
     (match-define (basic-id-formatter tid gsep nsep) af)
     (match-define (ast:group gid gparent gnodes ginfo) gs)
     (cond
       [(lookup-group-spec ts gparent)
        =>
        (λ (p)
          (format-id id "~a~a~a" p gsep id))]
       [else (format-id id "~a~a~a" tid gsep id)]))
   (define (format-group-arg-id af id ts gs)
     (id-without-type id))
   (define (format-node-id af id ts gs ns)
     (match-define (basic-id-formatter tid gsep nsep) af)
     (match-define (ast:group gid gparent gnodes ginfo) gs)
     (format-id id "~a~a~a" (format-group-id af gid ts gs) nsep id))
   (define (format-node-arg-id af id ts gs ns) (id-without-type id))])

(struct clean-id-formatter []
  #:methods gen:id-formatter
  [(define (format-group-id af id ts gs) id)
   (define (format-group-arg-id af id ts gs) (id-without-type id))
   (define (format-node-id af id ts gs ns) id)
   (define (format-node-arg-id af id ts gs ns) (id-without-type id))])

(define (get-formatter ast-id type)
  (match type
    [`(quote clean) (clean-id-formatter)]
    [else (basic-id-formatter ast-id #`: #`:)]))
