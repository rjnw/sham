#lang racket
(provide compiler)

(define compiler-procedure (lambda (c . args) ((compiler-func c) c args)))
(struct compiler [func] #:property prop:procedure (struct-field-index func))
