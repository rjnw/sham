#lang racket

(require "runtime/generics.rkt"
         "runtime/props.rkt"
         (for-syntax syntax/parse
                     "syntax/private/utils.rkt"))

(provide (all-defined-out)
         gmap gfold)

(struct ast:location [sloc tag] #:transparent)
(struct ast:metadata [locs custom] #:transparent)     ;; locs : tree of ast:location

(define (metadata-with-location vs loc (tag #f))
  (define (split lst f?)
    (for/fold ([t '()] [f '()]) ([v lst])
      (if (f? v) (values (cons v t) f) (values t (cons v f)))))
  (let*-values
      ([(mds mds^) (split vs ast:metadata?)]
       [(lcs lcs^) (split mds^ ast:location?)])
    (ast:metadata (cons (ast:location loc tag)
                        (append (map ast:metadata-locs mds)
                                lcs))
                  (match (append lcs^ (map ast:metadata-custom mds))
                    [(list c) c]
                    ['() (make-hash)]
                    [cs cs]))))

(define-syntax (generic-metadata stx)
  (syntax-parse stx
    [(_ (~optional (~seq (~datum #:tag) tag) #:defaults ([tag '#%default])) v ...)
     #`(metadata-with-location (list v ...) #,(syntax-srcloc stx) tag)]
    [_ #`(metadata-with-location (list) #,(syntax-srcloc stx) '#%default)]))

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

(struct ast:id ast [orig gen ;; scopes
                         ])
(struct ast:id:def ast:id [(maybe-refs #:mutable)])
(struct ast:id:ref ast:id [(maybe-def #:mutable)])
