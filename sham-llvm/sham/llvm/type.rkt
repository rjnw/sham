#lang racket/base
(require ffi/unsafe)

(define ((llvm-type? tag) v)
  (cpointer-has-tag? v tag))
