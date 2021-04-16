#lang racket

(require "spec.rkt"
         "pattern.rkt"
         syntax/parse
         (prefix-in rt: "runtime.rkt"))

(provide term-match-expander)

(define (pattern-expander stx pat)
  (parse-with-pattern pat stx))

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
       [(ast:node ids ninfo nargs pat)
        (define gs (find-node-group ts ss))
        (define gargs (full-group-args ts gs))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        (define nargs-stx (pattern-expander #`(#,@rest-stx) pat))
        #`(#,(ast:id-gen ids) (~? md _) (vector #,@gargs-stx) #,nargs-stx)]
       [(ast:group ids ginfo prnt gargs nodes)
        (error 'sham:sam "todo match expander for group types: ~a" (ast:id-orig ids))])]))

(module+ test
  (require rackunit))
