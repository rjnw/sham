#lang racket

(require syntax/parse)

(require "runtime.rkt"
         "spec.rkt")

(provide rkt-pattern-transformer)

(define (group-pattern-transformer gs as stx)
  (match-define (ast tid sid grps info) as)
  (match-define (ast:group (cons oid gsid) fid prnt args nds gi) gs)
  (syntax-parse stx
    [gid:id gsid]))

(define (node-pattern-transformer ns as stx)
  (match-define (ast tid sid grps info) as)
  (match-define (ast:node (cons oid nsid) fid args prnt ni) ns)
  (syntax-parse stx
    [nid:id nsid]
    [(_ args ...) (error 'sham:sag "TODO pattern constructor")]))

(define (rkt-pattern-transformer tt stx)
  (match-define (term-type t me ss ts) tt)
  (define-values (tsv _) (syntax-local-value/immediate ts))
  (cond [(ast:node? ss) (node-pattern-transformer ss tsv stx)]
        [(ast:group? ss) (group-pattern-transformer ss tsv stx)]))
