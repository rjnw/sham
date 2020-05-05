#lang racket

(provide raw-type raw-val
         sym-type sym* sym-val sym sym-case e-sym-case
         bool true false
         vector list
         to-rkt-type)

(require sham/ir/ast/specific
         sham/ir/ast/simple
         sham/ir/ast/syntax
         sham/ir/derived
         sham/ir/ast/core
         sham/llvm/ir/ast
         sham/md
         (prefix-in rkt- sham/rkt/conv))

(require (prefix-in rkt: racket)
         (for-syntax racket/syntax syntax/parse)
         ffi/unsafe
         syntax/parse/define)

(define raw-type
  (sham-metadata! (t-ref 'i64)
                  (set-type-md-special-rkt! (empty-type-md)
                                            (make-ctype _uint64 rkt-raw-uintptr rkt-unraw-uintptr))))
(define (raw-val v)
  (v-ui (rkt-raw-uintptr v) raw-type))

(define sym-type raw-type)
(define sym-val raw-val)

(define-syntax (sym stx)
  (syntax-parse stx
    [(_ v:id) #`(sym-val `v)]
    [(_ v) #`(sym-val v)]
    [(~literal sym) #`sym-type]))
(define sym* (t-pointer sym-type))

(define bool raw-type)
(define true (raw-val rkt:true))
(define false (raw-val rkt:false))

(define-simple-macro (sym-case test [(~or single-datum:id (datum:id ...)) body ...] ... [(~datum else) default])
  (m-switch^ test [(~? ((sym single-datum)) ((sym datum) ...)) (block^ body ...)] ... default))

(define-simple-macro (e-sym-case type test [(datum:id ...) body] ... [(~datum else) default])
  (e-m-switch^ type test [((sym datum) ...) body] ... default))

(define (from-llvm-type sym)
  (match sym
    ['i1 _bool]
    ['i8 _uint8]
    ['i16 _uint16]
    ['i32 _uint32]
    ['i64 _uint64]
    ['f32 _float]
    ['f64 _double]
    ['void _void]
    [else (error 'sham:rkt:type "error unknown type reference ~a" sym)]))

(define (to-rkt-type t-ast (def-map #f))
  (define (rec t)
    (define specialized-type (ref-type-md-special-rkt (sham-metadata t)))
    (or specialized-type
        (match t
          [(llvm:ast:type:ref md to)
           (cond
             [(from-llvm-type to) => identity]
             [(and (hash? def-map)
                   (hash-ref def-map to #f))
              =>
              (λ (d)
                (match d
                  [(llvm:def:type _ _ t) (or (ref-type-md-special-rkt md)
                                             (rec t))]
                  [(sham:def:struct md _ _ _) (ref-type-md-special-rkt md)]))])]
          [(llvm:ast:type:pointer _ _) _pointer]
          [(llvm:ast:type:array _ _ _) _pointer]
          [(llvm:ast:type:function _ args var-arg? ret)
           (define arg-types (map rec args))
           (define ret-type (rec ret))
           (if (or (ormap false? arg-types) (false? ret-type))
               (error 'sham:rkt:type
                      "error one of the type for a function cannot be converted to racket ~a:~a=~a:~a"
                      args ret arg-types ret-type)
               (_cprocedure arg-types ret-type))]
          [else #f])))
  (rec t-ast))

(define (vector type rkt-vector)
  (define rkt-type (to-rkt-type type))
  (unless (ctype? rkt-type)
    (error 'sham:ir:rkt "rkt-vector, couldn't convert sham type to rkt type ~a" type))
  (vector->cblock rkt-vector rkt-type))

(define (list type rkt-list)
  (define rkt-type (to-rkt-type type))
  (unless (ctype? rkt-type)
    (error 'sham:ir:rkt "rkt-list, couldn't convert sham type to rkt type ~a" type))
  (list->cblock rkt-list rkt-type))
