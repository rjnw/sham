#lang racket
(require "reqs.rkt")
(provide (all-defined-out))

(define (get-current-ast-group state)
  #f)

(define (add-ooo-stx-path state k)
  (define (in-path p) (cons k p))
  (match-define (cmplr:state:node spec-state vars path) state)
  (cmplr:state:node spec-state vars (in-path path)))

(define (peel-ooo-stx-path state)
  (define (peel-path p) (cdr p))
  (match-define (cmplr:state:node spec-state vars path) state)
  (cmplr:state:node spec-state vars (peel-path path)))
