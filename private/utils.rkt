#lang racket

(require racket/splicing)
(provide (all-defined-out))

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))
(define sham-diagnose (make-parameter #f))

(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1))
         (symbol->string s2)))
(require "info-key.rkt")
(provide (all-defined-out))
