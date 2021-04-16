#lang racket

(require racket/generic
         racket/syntax
         syntax/parse)
(require "spec.rkt"
         "syntax-class.rkt"
         "utils.rkt")

(provide (contract-out (format-group-id (-> id-formatter? identifier? ast? ast:group? identifier?))
                       (format-node-id (-> id-formatter? identifier? ast? ast:group? ast:node? identifier?)))
         get-formatter)

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
     (define (lookup-group-spec ts p) (if p (assoc-default (->symbol p) (ast-groups ts)) p))
     (let recf ([p gparent]
                [c id])
       (format-id c "~a~a~a"
                  (match (lookup-group-spec ts p)
                    [#f tid]
                    [(ast:group pgid pgparent _ _) (recf pgparent pgid)])
                  gsep c)))
   (define (format-node-id af id ts gs ns)
     (match-define (basic-id-formatter tid gsep nsep) af)
     (match-define (ast:group gid gparent gnodes ginfo) gs)
     (format-id id "~a~a~a" (format-group-id af gid ts gs) nsep id))])

(struct clean-id-formatter []
  #:methods gen:id-formatter
  [(define (format-group-id af id ts gs) id)
   (define (format-node-id af id ts gs ns) id)])

(define (get-formatter ast-id type)
  (match type
    [`(quote clean) (clean-id-formatter)]
    [else (basic-id-formatter ast-id #`: #`:)]))
