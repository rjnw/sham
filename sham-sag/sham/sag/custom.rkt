#lang racket

(require
 (for-syntax
  "private/ast-syntax-structs.rkt"
  "private/ast-syntax-class.rkt"
  "private/ast-syntax-generics.rkt"
  syntax/parse
  racket/syntax
  racket/pretty))

(provide (all-defined-out))
(define-syntax (map-generic ast-id ast-spec)
  (printf "hello from map-generic\n")
  #f)
(define-syntax (sexp-printer ast-id ast-spec)
  (printf "hello from sexp-printer\n")
  #f)
