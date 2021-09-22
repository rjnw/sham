#lang racket

(require racket/stxparam)

(provide (all-defined-out))

(define transform-procedure (lambda (c . args) ((transform-func c) c args)))
(struct transform [func] #:property prop:procedure (struct-field-index func))

(define-syntax-parameter ^ #f)
(define-syntax-parameter with #f)

(define (identifier->syntax ident stx-ctxt)
  (cond
    [(symbol? ident) (datum->syntax stx-ctxt ident)]
    [else (error 'sham/sam/TODO "compile new identifier")]))
