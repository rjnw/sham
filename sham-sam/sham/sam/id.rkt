#lang racket

(require "runtime.rkt")
(provide (all-defined-out))

(define (id-orig=? i1 i2)
  (equal? (ast:id-orig i1) (ast:id-orig i2)))
(define (id-gen=? i1 i2)
  (equal? (ast:id-gen i1) (ast:id-gen i2)))

(define (id-def=? i1 i2)
  (and (ast:id:def? i1) (ast:id:def? i2) (id-orig=? i1 i2) (id-ref=? i1 i2)))
(define (id-ref=? i1 i2)
  (and (ast:id:ref? i1) (ast:id:ref? i2) (id-orig=? i1 i2) (id-ref=? i1 i2)))
(define (id=? i1 i2)
  (or (equal? i1 i2) (id-def=? i1 i2) (id-ref=? i1 i2)))
(define (any-id=? i1 i2)
  (or (id=? i1 i2)
      (and (and (ast:id? i1) (ast:id? i2))
           (or (id-gen=? i1 i2) (id-orig=? i1 i2)))))

(define (build-id-map alist)
  (cond [(list? alist) alist]
        [(hash? alist) (for/list ([(i v) alist]) (cons i v))]
        [else (error 'sham/sam "unknown value for building id map ~a" alist)]))

(define (id-map-add alist id val (=? id=?) #:override (ovrd #t))
  (if ovrd
      (cons (cons id val) alist)
      (match alist
        [(cons (cons i v) rst)
         #:when (=? i id)
         (cons (cons i val) rst)]
        [(cons iv rst)
         (cons iv (id-map-add rst id val))]
        ['() (cons id val)])))

(define (id-map-ref alist id (=? any-id=?))
  (match (assoc id alist =?)
    [#f #f]
    [(cons id val) val]))

(define (build-id-binding-map binding-list) 'TODO)
(define (id-binding-map-add binding-map id value) 'TODO)
