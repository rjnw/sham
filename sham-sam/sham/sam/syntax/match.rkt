#lang racket

(require "spec.rkt"
         "pattern.rkt"
         syntax/parse
         (prefix-in rt: "runtime.rkt"))

(provide term-match-expander)

(define (pattern-expander stx pat)
  (fold-with-pattern pat stx))

(define (match-group-args gargs stxs)
  (match* (gargs stxs)
    [('() ss) (values '() ss)]
    [((cons gs grs) (cons ss srs))
     (define-values (gr sr) (match-group-args grs srs))
     (values (cons ss gr) sr)]
    [(_ _) (error 'sham/sam "not enough arguments for common group args")]))

(define (term-match-expander tt stx)
  (match-define (rt:term-type rt mt ss ts) tt)
  (syntax-parse stx
    [(_ (~optional (~seq (~datum #:md) md)) args ...)
     (match ss
       [(ast:node (ast:id nid-o nid-g nid-f) nargs pat ninfo)
        (define gs (lookup-node-group ts ss))
        (define gargs (full-group-args ts gs))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        (define nargs-stx (pattern-expander #`(#,@rest-stx) pat))
        #`(#,nid-g (~? md _) (vector #,@gargs-stx) #,nargs-stx)]
       [(ast:group (ast:id gid-o gid-g gid-f) prnt gargs nodes ginfo)
        (error 'sham:sam "todo match expander for group types: ~a" (car gid-o))])]))

(module+ test
  (require rackunit))
