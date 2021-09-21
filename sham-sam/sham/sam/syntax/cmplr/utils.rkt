#lang racket

(require "../spec.rkt")

(provide (all-defined-out))

(define node-pat-operators 'node-pat-operators)
(define node-body-operators 'node-body-operators)

(define (get-cmplr-type stx)
  (match (syntax-e stx)
    [name (lookup-spec stx)]
    [(cons op args)
     #:when (identifier? op)
     ((lookup-spec op) args)]))

;; (define ((find-operators operators get-identifier) stxid)
;;   (define (is-op? bo) (free-identifier=? (operator-identifier bo) stxid))
;;   (filter operators is-op?))

;; (stx pat -> (values stx (listof dirs))) (listof stx) -> (values (listof stx) (listof dirs))
;; special map over lists with 2 values returning where second value is a list which is accumulated

;; (define (map-with-dirs f lst)
;;   (for/fold ([stxs '()]
;;              [dirs '()]
;;              #:result (values (reverse stxs) dirs))
;;             ([sub lst])
;;     (define-values (sub-stx sub-dirs) (f sub))
;;     (values (cons sub-stx stxs)
;;             (append dirs sub-dirs))))
