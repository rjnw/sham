#lang racket

(provide sym sym*
         s-case e-case
         bool true false
         vector list)

(require (prefix-in rkt- sham/rkt/conv))

(require (prefix-in rkt: racket)
         (for-syntax racket/syntax))

(define type-sym i64)
(define-syntax (sym stx)
  (syntax-parse stx
    [(_ v) (rkt-raw-uintptr `v)]
    [(~literal sym) type-sym]))

(define type-sym* i64*)

(define bool i64)
(define true (rkt-raw-uintptr rkt:true))
(define false (rkt-raw-uintptr rkt:false))

(define type-ri64 i64)

(define-syntax (ri64 stx)
  (syntax-parse stx
    [(_ v) (ui64 (untag-int v))]
    [(~literal ri64) type-ri64]))

(define-simple-macro (sym-case type test [(datum:id ...) body] ... [(~datum else) default])
  (e-m-switch^ type test [((sym datum) ...) body] ... default))

(define (vector type rkt-vector)
  (define rkt-type (sham-type->rkt-type type))
  (unless (ctype? rkt-type)
    (error 'sham:ir:rkt "rkt-vector, couldn't convert sham type to rkt type ~a" type))
  (vector->cblock rkt-vector rkt-type))

(define (list type rkt-list)
  (define rkt-type (sham-type->rkt-type type))
  (unless (ctype? rkt-type)
    (error 'sham:ir:rkt "rkt-vector, couldn't convert sham type to rkt type ~a" type))
  (list->cblock rkt-vector rkt-type))
