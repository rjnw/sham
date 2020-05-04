#lang racket

(provide (all-defined-out))

(define (list-box args) (box args))

(define list-unbox unbox)

(define ((add-to-list-box! b) v)
  (set-box! b (cons v (list-unbox b))))
