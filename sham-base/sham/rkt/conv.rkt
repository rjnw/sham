#lang racket

(require ffi/unsafe)

(provide (all-defined-out))

(define (raw-uintptr value)
  (printf "raw-uintptr: ~a\n" value)
  (cast value _scheme _uintptr))
(define (unraw-uintptr uintptr) (cast uintptr _uintptr _scheme))

(define (func-uintptr value type)
  (cast (function-ptr value type) _pointer _uintptr))

(define (untag-int i)
  (if (eq? (system-type 'vm) 'racket)
      (bitwise-ior (arithmetic-shift i 1) 1)
      (arithmetic-shift i 3)))

(define (tag-int i)
  (if (eq? (system-type 'vm) 'racket)
      (arithmetic-shift i -1)
      (arithmetic-shift i -3)))

(define (raw-internal name) (get-ffi-obj name #f _uintptr))
