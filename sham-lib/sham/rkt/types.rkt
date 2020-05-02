#lang racket

(require sham/llvm/ir/ast
         sham/ir/ast/core
         sham/private/md
         sham/ir/env)
(require ffi/unsafe)

(provide rkt-jit-cast)

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

(define ((build-rkt-types s-env))
  (match-define (sham-env s-mod ll-env s-md) s-env)
  (match-define (sham-module s-ast ll-ast ext) s-mod)
  (match-define (sham:def:module m-info mod-id defs) s-ast)
  (define def-map (for/hash ([d defs])
                    (values (if (sham:def? d) (sham:def-id d) (llvm:def-id d)) d)))
  (for/hash ([d defs]
             #:when (sham:def:function? d))
    (match-define (sham:def:function f-md f-name t-ast f-body) d)
    (values f-name (or (function-md-jit-rkt-type f-md) (to-rkt-type t-ast def-map)))))

(define (try-populate-rkt-types! s-env)
  (match-define (sham-env s-mod ll-env s-md) s-env)
  (md-ref! s-md sham-env-md-rkt-types
           (build-rkt-types s-env)))

(define (sham-env-rkt-type s-env name)
  (match-define (sham-env s-mod ll-env s-md) s-env)
  (try-populate-rkt-types! s-env)
  (hash-ref (ref-sham-env-md-rkt-types s-md) name))

(define (uintptr->ptr->rkt uintptr type)
  (cast (cast uintptr _uintptr _pointer) _pointer type))

(define (rkt-jit-cast s-env name uintptr)
  (define rkt-type (sham-env-rkt-type s-env name))
  (uintptr->ptr->rkt uintptr rkt-type))
