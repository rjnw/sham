#lang racket

(require "runtime/generics.rkt"
         "runtime/props.rkt"
         (for-syntax syntax/parse))

(provide (all-defined-out)
         gmap gfold)

(struct ast:location:union [of])        ;; (list/c (cons/c symbol:tag srcloc?))
(struct ast:metadata [loc custom])

(struct ast [md])
(struct ast:group ast [args])

(struct ast:term ast:group [args]
  #:methods gen:term:fold
  [(define (gfold ff f v)
     (match-define (ast:term md gas tas) v)
     (define ngas ((gfold-rec-vl ff f) gas))
     (define ntas ((gfold-rec-vl ff f) tas))
     (cond [(ff v) => (lambda (ff^) (ff^ ngas ntas))]
           [(has-ast-constructor? v) ((get-ast-constructor v) md ngas ntas)]))]
  #:methods gen:term:map
  [(define (gmap f v)
     (match-define (ast:term md gas tas) v)
     ((if (has-ast-constructor? v) ((get-ast-constructor v)) ast:term)
      md
      ((gmap-rec-vl f) gas)
      ((gmap-rec-vl f) tas)))])
