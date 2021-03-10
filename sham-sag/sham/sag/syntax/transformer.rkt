#lang racket

(require syntax/parse
         (for-template racket))

(require "runtime.rkt"
         "spec.rkt"
         "pattern.rkt")

(provide rkt-pattern-transformer)

(define (group-pattern-transformer gs as stx)
  (match-define (ast tid sid grps info) as)
  (match-define (ast:group (cons oid gsid) fid prnt args nds gi) gs)
  (syntax-parse stx
    [gid:id gsid]))

(define (node-pattern-transformer ns as stx)
  (match-define (ast tid sid grps info) as)
  (match-define (ast:node (cons oid nsid) fid args pat ni) ns)
  (syntax-parse stx
    [nid:id nsid]
    [(_ (~optional (~seq (~datum #:md) md:expr)) args ...)
     #`(#,nsid (~? md #f) (vector) #,(map-with-pattern pat #`(args ...)))]))

(define (rkt-pattern-transformer tt stx)
  (match-define (term-type t me ss tsv) tt)
  (cond [(ast:node? ss) (node-pattern-transformer ss tsv stx)]
        [(ast:group? ss) (group-pattern-transformer ss tsv stx)]))
