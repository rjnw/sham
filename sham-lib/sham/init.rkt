#lang racket

(require sham/llvm/ffi/all)

(provide initialize)

(define initialized? #f)

(define (initialize)
  (unless initialized?
    (llvm-initialize-all)
    (set! initialized? #t)))
