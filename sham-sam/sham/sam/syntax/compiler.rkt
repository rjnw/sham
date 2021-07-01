#lang racket

(require
 "syntax/spec.rkt"
 (submod "syntax/private/spec.rkt" compiler)
 (submod "syntax/private/syntax-class.rkt" compiler))

(define-generics compiler-pattern
  (expand-pattern stx input-zipper)
  (perform-pattern stx output-zipper)
  )
