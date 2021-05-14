#lang racket

(require "runtime/generics.rkt"
         "runtime/props.rkt"
         (for-syntax syntax/parse
                     "syntax/private/syntax-class.rkt"
                     "syntax/private/utils.rkt"))

(provide (all-defined-out)
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
     #`(metadata-with-location v #,(info-value 'srcloc inf (syntax-srcloc stx)))]))

(struct ast [md])
(struct ast:group ast [args])

(struct ast:term ast:group [args]
  #:methods gen:term:fold
  [(define (gfold ff f v)
     (match-define (ast:term md gas tas) v)
     (define ngas ((gfold-rec-vl ff f) gas))
     (define ntas ((gfold-rec-vl ff f) tas))
     (cond [(ff v) => (lambda (ff^) (ff^ ngas ntas))]
           [(has-ast-constructor? v) (((get-ast-constructor v)) md ngas ntas)]))]
  #:methods gen:term:map
  [(define (gmap f v)
     (match-define (ast:term md gas tas) v)
     ((if (has-ast-constructor? v) (((get-ast-constructor v))) ast:term)
      md
      ((gmap-rec-vl f) gas)
      ((gmap-rec-vl f) tas)))])

(struct ast:scope [kind instance])
(struct ast:id ast [orig gen scope])
(struct ast:id:def ast:id [(maybe-refs #:mutable)])
(struct ast:id:ref ast:id [(maybe-def #:mutable)])
