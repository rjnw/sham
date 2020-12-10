#lang racket

(require racket/generics)

(provide (all-defined-out))

(define-generics ast-builder
  (build-group-struct ast-builder group-syntax group-spec)
  (build-group-extra ast-builder group-spec)
  (build-node-struct ast-builder node-syntax group-spec node-spec)
  (build-node-extra ast-builder group-spec node-spec)
  (build-syntax ast-builder full-syntax)
  #:defaults
  ([any/c
    (define (build-group-struct ab syn gs) syn)
    (define (build-group-extra ab gs) #f)
    (define (build-node-struct ab syn gs ns) syn)
    (define (build-node-extra ab syn gs ns) syn)
    (define (build-syntax b fmt) #f)]))
