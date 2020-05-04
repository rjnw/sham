#lang racket

(provide type-sym sym sym-case e-sym-case sym*
         bool true false
         type-ri64
         vector list
         to-rkt-type)

(require sham/ir/ast/specific
         sham/ir/ast/simple
         sham/ir/derived
         sham/ir/ast/core
         sham/llvm/ir/ast
         sham/md
         (prefix-in rkt- sham/rkt/conv))

(require (prefix-in rkt: racket)
         (for-syntax racket/syntax syntax/parse)
         ffi/unsafe
         syntax/parse/define)

(define type-sym (sham-metadata! i64 (set-type-md-special-rkt! (empty-type-md)
                                                               (make-ctype _uint64 rkt-raw-uintptr rkt-unraw-uintptr))))
(define-syntax (sym stx)
  (syntax-parse stx
    [(_ v) #`(ui64 (rkt-raw-uintptr `v))]
    [(~literal sym) #`type-sym]))
(define sym* (t-pointer type-sym))

(define bool i64)
(define true (ui64 (rkt-raw-uintptr rkt:true)))
(define false (ui64 (rkt-raw-uintptr rkt:false)))

(define type-ri64 i64)

(define-syntax (ri64 stx)
  (syntax-parse stx
    [(_ v) #`(ui64 (untag-int v))]
    [(~literal ri64) #`type-ri64]))

(define-simple-macro (sym-case test [(datum:id ...) body ...] ... [(~datum else) default])
  (m-switch^ type test [((sym datum) ...) body ...] ... default))

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
