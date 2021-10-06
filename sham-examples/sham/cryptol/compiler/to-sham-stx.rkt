#lang racket

(require syntax/parse
         racket/syntax)

(require "../ast.rkt"
         sham/sam/transform
         sham/sam/rkt)

(provide cry-ast-to-sham-stx)

(define-transform (cry-ast-to-sham-stx)
  (cry-ast -> (stx sham)))
