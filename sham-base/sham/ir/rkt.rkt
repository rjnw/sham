#lang racket

(provide raw-type raw-val
         sym-type sym-val sym sym-case e-sym-case
         bool true false
         array-from-vector array-from-list
         to-rkt-type)

(require sham/ir/simple
         sham/ir/ast
         sham/llvm/ir/ast
         (prefix-in ll- sham/llvm/ir/simple)
         sham/md
         (prefix-in rkt- sham/rkt/conv))

(require (prefix-in rkt: racket)
         (for-syntax racket/syntax syntax/parse)
         ffi/unsafe
         syntax/parse/define)

;; TODO cs does not support ptr to non-atomic memory
(define raw-type
  (ll-type-ref #:md (set-type-md-special-rkt! (empty-type-md)
                                           (make-ctype _uint64 rkt-raw-uintptr rkt-unraw-uintptr))
            'i64))

(define (raw-val v)
  (unless (exact-positive-integer? v)
    (printf "converting incorrect raw value for llvm: ~a\n" v))
  (ll-val-ui v raw-type))

(define sym-type raw-type)
(define sym-val raw-val)

(define-syntax (sym stx)
  (syntax-parse stx
    [(_ v:id) #`(sym-val `v)]
    [(_ v) #`(sym-val v)]
    [(~literal sym) #`sym-type]))
(define sym* (ll-type-pointer sym-type))

(define bool-type raw-type)
(define bool-val raw-val)

(define-syntax (bool stx)
  (syntax-parse stx
    [(_ v:id) #`(bool-val v)]
    [(~literal bool) #`bool-type]))
;; (define true (bool-val rkt:true))
;; (define false (bool-val rkt:false))

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
          [(llvm:type:ref #:md md to)
           (cond
             [(from-llvm-type to) => identity]
             [(and (hash? def-map)
                   (hash-ref def-map to #f))
              =>
              (λ (d)
                (match d
                  [(llvm:def:type _ t) (or (ref-type-md-special-rkt md)
                                             (rec t))]
                  [(sham:def:struct #:md md _ _ _) (ref-type-md-special-rkt md)]))])]
          [(llvm:type:pointer _) _pointer]
          [(llvm:type:array _ _) _pointer]
          [(llvm:type:function args var-arg? ret)
           (define arg-types (map rec args))
           (define ret-type (rec ret))
           (if (or (ormap false? arg-types) (false? ret-type))
               (error 'sham:rkt:type
                      "error one of the type for a function cannot be converted to racket ~a:~a=~a:~a"
                      args ret arg-types ret-type)
               (_cprocedure arg-types ret-type))]
          [else #f])))
  (rec t-ast))

(define (array-from-vector type rkt-vector)
  (define rkt-type (to-rkt-type type))
  (unless (ctype? rkt-type)
    (error 'sham:ir:rkt "rkt-vector, couldn't convert sham type to rkt type ~a" type))
  (vector->cblock rkt-vector rkt-type))

(define (array-from-list type rkt-list)
  (define rkt-type (to-rkt-type type))
  (unless (ctype? rkt-type)
    (error 'sham:ir:rkt "rkt-list, couldn't convert sham type to rkt type ~a" type))
  (list->cblock rkt-list rkt-type))
