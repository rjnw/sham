#lang racket
(provide (all-defined-out))

(define (fninfo-empty)
  (make-immutable-hash))
(define fn-attr-key 'fn)
(define fn-ret-attr-key 'ret)
(define (fn-arg-attr-key n) `(arg ,n))
(define fn-md-key 'md)
(define fn-pass-key 'pass)
(define function-attribute-index (modulo -1 (expt 2 32)))

(define (add-info info key lst)
  (hash-set info key (append (hash-ref info key '()) lst)))

(define (fninfo-add-attrs info . attrs) (add-info info fn-attr-key attrs))
(define (fninfo-add-ret-attrs info . attrs) (add-info info fn-ret-attr-key attrs))
(define (fninfo-add-argi-attrs info argn . attrs) (add-info info (fn-arg-attr-key argn) attrs))
(define (fninfo-add-md info . mds) (add-info info fn-md-key mds))
(define (fninfo-add-passes info . passes) (add-info info fn-pass-key passes))
