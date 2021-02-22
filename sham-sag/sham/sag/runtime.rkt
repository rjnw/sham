#lang racket

(require racket/generic
         (for-syntax syntax/parse))

(provide (all-defined-out))

(begin-for-syntax
  (struct term-type [rename-transformer match-expander self-spec top-spec]
    #:property prop:rename-transformer (struct-field-index rename-transformer)
    #:property prop:match-expander (struct-field-index match-expander)))

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
(struct ast:metadata [loc])

(struct ast [md])
(struct ast:group ast [args])

(struct ast:term ast:group [vals]
  #:methods gen:gterm
  [(define/generic super-gmap-t gmap-t)
   (define (gmap-t ff f v)
     (match-define (ast:term md args vals) v)
     ((ff v) (super-gmap-t ff f args) (super-gmap-t ff f vals)))])


;; (define-syntax (define-transformation stx)
;;   (syntax-parse stx
;;     [(_ tid:id astid:id (from:id (~datum ->) to:id)
;;         ts:transform ...)
;;      ])
;;   )

(define-syntax (define-reduction stx)
  #`(void))
