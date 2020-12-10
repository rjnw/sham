#lang racket

(require racket/generic
         (for-syntax syntax/parse))

(provide (all-defined-out))

(struct ast-metadata [loc])
(define-generics term
  ;; gmap-t : (-> a (-> b ... c)) (-> x b) a)
  ;; ((ff term) ((f <term-internal-args>) ...))
  (gmap-t ff f term)
  #:defaults ([list?
               (define (gmap-t ff f term)
                 (apply (ff term) (map f term)))]))

(define-syntax (define-transformation stx)
  #`(void)
  ;; (syntax-parse stx
  ;;   [(_ tid:id typ:type ts:transform ...)
  ;;    ])
  )

(define-syntax (define-query stx)
  #`(void))
