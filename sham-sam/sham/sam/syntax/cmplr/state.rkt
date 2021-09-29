#lang racket
(require "reqs.rkt")
(provide (all-defined-out))

(struct ast-path [pat depth])

(define (add-ooo-stx-path state k)
  (define (in-path p) (cons k p))
  (match-define (cmplr:state:node spec-state vars path) state)
  (cmplr:state:node spec-state vars (in-path path)))

(define (peel-ooo-stx-path state)
  (define (peel-path p) (cdr p))
  (match-define (cmplr:state:node spec-state vars path) state)
  (cmplr:state:node spec-state vars (peel-path path)))

(define (append-dir-in-state dir state)
  (match-define (cmplr:state:node spec-state dirs path) state)
  (cmplr:state:node spec-state (append dirs (list dir)) path))
