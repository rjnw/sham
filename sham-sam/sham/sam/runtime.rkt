#lang racket

(require "runtime/generics.rkt"
         "runtime/props.rkt"
         "runtime/ast.rkt"
         "runtime/identifier.rkt"
         (for-syntax syntax/parse
                     "syntax/kw-info.rkt"
                     "syntax/utils.rkt"))

(provide (all-defined-out)
         (all-from-out "runtime/ast.rkt"
                       "runtime/identifier.rkt")
         gmap gfold)

(struct ast:location [sloc tag] #:transparent)
(struct ast:metadata [locs custom] #:transparent)     ;; locs : tree of ast:location

(define (metadata-with-location vs loc (info '()))
  (match vs
    [(ast:metadata locs cstm) (ast:metadata (cons loc locs)
                                            (append info cstm))]
    [else (ast:metadata loc (cons vs info))]))

(define-syntax (generic-metadata stx)
  (syntax-parse stx
    [(_ (~optional v #:defaults ([v #f])) kws:keyword-info)
     (define inf (attribute kws.spec))
     #`(metadata-with-location v #,(info-1value 'srcloc inf (syntax-srcloc stx)))]))
