#lang racket

(require racket/stxparam)
(require "identifier.rkt")

(provide (all-defined-out))

;; (define transform-procedure (lambda (c . args) ((transform-func c) c args)))
;; (struct transform [func] #:property prop:procedure (struct-field-index func))

(define-syntax-parameter ^ #f)
;; (define-syntax-parameter with #f)

(define (identifier->syntax ident) (ast-id-stxid ident))

(define (compile-identifier-def ident (scope default-id-scope))
  (define stxid ident)
  (define gen-stxid (car (generate-temporaries (list stxid))))
  (ast:id:def stxid stxid gen-stxid scope '()))

(define (compile-identifier-ref ident (scope default-id-scope) (maybe-scope-map #f))
  (define maybe-def (find-def-for-id ident scope maybe-scope-map))
  (define ref (ast:id:ref ident ident maybe-def))
  (when maybe-def (add-id-ref! maybe-def ref))
  ref)