#lang racket

(require racket/generic
         (for-syntax syntax/parse))

(provide (all-defined-out))

(define-generics gterm
  ;; gmap-t : (-> a (-> b ... c)) (-> x b) a)
  ;; ((ff term) ((f <term-internal-args>) ...))
  (gmap-t ff f gterm)
  #:defaults ([list?
               (define (gmap-t ff f term)
                 (apply (ff term) (map f term)))]
              [vector?
               (define (gmap-t ff f term)
                 (apply (ff term) (vector-map f term)))]))


(struct ast:location:union [of])        ;; (list/c (cons/c symbol:tag srcloc?))
(struct ast:metadata [loc custom])

(struct ast [md])
(struct ast:group ast [args])

(struct ast:term ast:group [vals]
  #:methods gen:gterm
  [(define/generic super-gmap-t gmap-t)
   (define (gmap-t ff f v)
     (match-define (ast:term md args vals) v)
     ((ff v) (super-gmap-t ff f args) (super-gmap-t ff f vals)))])
