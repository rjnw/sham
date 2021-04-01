#lang racket

(require racket/generic)

(provide (all-defined-out))

(define-generics term:fold
  ;; gfold : (-> a (-> b ... c)) (-> x b) a)
  ;; ((ff term) ((f <term-internal-args>) ...))
  (gfold ff f term:fold)
  #:defaults ([list? (define (gfold ff f term) (apply (ff term) (map f term)))]
              [vector? (define (gfold ff f term) (apply (ff term) (vector->list (vector-map f term))))]))


(define-generics term:map
  (gmap f term:map)
  #:fast-defaults ([list? (define (gmap f term) (map f term))]
                   [vector? (define (gmap f term) (vector-map f term))]))

(define ((gfold-rec ff f) v) (if (term:fold? v) (gfold ff (gfold-rec ff f) v) (f v)))
(define ((gmap-rec f) v) (if (term:map? v) (gmap (gmap-rec f) v) (f v)))
(define ((gmap-rec-bf f) v) (if (term:map? v) (gmap (gmap-rec-bf f) (f v)) (f v)))
(define ((gmap-rec-df f) v) (if (term:map? v) (f (gmap (gmap-rec-df f) v)) (f v)))

(define ((gfold-rec-vl ff f) v) (if (or (vector? v) (list? v)) (gfold ff (gmap-rec-vl ff f) v) (f v)))
(define ((gmap-rec-vl f) v) (if (or (vector? v) (list? v)) (gmap (gmap-rec-vl f) v) (f v)))
