#lang racket

(require "../spec.rkt"
         (submod "../spec.rkt" compiler))

(provide (all-defined-out))

(define (get-cmplr-type stx)
  (match (syntax-e stx)
    [name (lookup-spec stx)]
    [(cons op args)
     #:when (identifier? op)
     ((lookup-spec op) args)]))

(define (cmplr-args-id cspec)
  (map car (cmplr:header-args (cmplr-header cspec))))

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


;; info keys

(define ik-node-pat-ops 'node-pattern-operators)
(define ik-node-body-ops 'node-body-operators)

(define ik-node-bs 'node-builders)
(define ik-node-pat-bs 'node-pattern-builders)
(define ik-node-body-bs 'node-body-builders)
(define ik-group-bs 'group-builders)
(define ik-top-bs 'top-builders)

(define ik-spec-bs 'spec-builders)
