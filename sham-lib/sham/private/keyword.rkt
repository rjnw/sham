#lang racket

(require sham/private/env)
(require (for-syntax syntax/parse))

(provide define-general-keyword-procedure
         lookup-keyword)

(define-syntax (lookup-keyword stx)
  (syntax-parse stx
    [(_ kassoc (~or k:keyword ((~literal quote) kq:keyword)) ... (~optional defaultf))
     #`(assoc-default kassoc `(k ... kq ...) (~? defaultf))]
    [(_ kassoc ks (~optional defaultf))
     #`(assoc-default kassoc ks (~? defaultf))]))

(define-syntax (define-general-keyword-procedure stx)
  (syntax-parse stx
    [(_ (name:id kassoc:id args:id ... . rest-args:id)
        body ...)
     #`(define name
         (make-keyword-procedure
          (λ (kws kw-args args ... . rest-args)
            (define kassoc (map cons kws kw-args))
            body ...)))]))
