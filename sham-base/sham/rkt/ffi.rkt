#lang racket

(require ffi/unsafe)

(provide get-lib-uintptr)

(define (get-lib-uintptr ffi-lib fname)
  (cast (ffi-obj-ref fname ffi-lib) _pointer _uintptr))
