#lang racket

(require syntax/parse
         (for-template racket))

(require "runtime.rkt"
         "spec.rkt"
         "pattern.rkt")

(provide rkt-pattern-transformer)

#;(- there are three different options for providing macros for production nodes
     * generate a syntax macro for each production
     * write a conventional pattern matcher that performs
     * make use of racket's pattern matcher and convert our patterns to racket based on the storage format

     We use the third option here using fold-with-pattern which takes a pattern and syntax folds
     over the syntax according to the pattern)


(define (group-pattern-transformer gs as stx)
  (match-define (ast tid tids grps info) as)
  (match-define (ast:group gids ginfo gparent args nds) gs)
  (syntax-parse stx
    [gid:id (get-struct-id gids)]))

(define (node-pattern-transformer ns as stx)
  (match-define (ast tid tids grps info) as)
  (match-define (ast:node nids ninfo args pat) ns)
  (syntax-parse stx
    [nid:id (get-struct-id nids)]
    [(_ (~optional (~seq (~datum #:md) md:expr)) args ...)
     #`(#,(get-struct-id nids) (~? md #f) (vector) #,(parse-with-pattern pat #`(args ...)))]))

(define (rkt-pattern-transformer tt stx)
  (match-define (term-type t me ss tsv) tt)
  (cond [(ast:node? ss) (node-pattern-transformer ss tsv stx)]
        [(ast:group? ss) (group-pattern-transformer ss tsv stx)]))
