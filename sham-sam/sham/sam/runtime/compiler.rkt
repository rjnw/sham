#lang racket

(require racket/stxparam)

(provide (all-defined-out))

(define compiler-procedure (lambda (c . args) ((compiler-func c) c args)))
(struct compiler [func] #:property prop:procedure (struct-field-index func))

(define-syntax-parameter ^ #f)
(define-syntax-parameter with #f)
