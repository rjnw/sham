#lang racket

(require sham/llvm/ir/ast
         sham/ir/ast/core
         sham/ir/rkt
         sham/md
         sham/ir/env)
(require ffi/unsafe)

(provide rkt-cast-jit-ptr)

(define ((build-rkt-types s-env))
  (match-define (sham-env s-mod ll-env s-md) s-env)
  (match-define (sham-module s-ast ll-ast ext) s-mod)
  (match-define (sham:def:module m-info mod-id defs) s-ast)
  (define def-map (for/hash ([d defs])
                    (values (if (sham:def? d) (sham:def-id d) (llvm:def-id d)) d)))
  (for/hash ([d defs]
             #:when (sham:def:function? d))
    (match-define (sham:def:function f-md f-name t-ast f-body) d)
    (values f-name (or (ref-function-md-jit-rkt-type f-md) (to-rkt-type t-ast def-map)))))

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

(define (rkt-cast-jit-ptr s-env name uintptr)
  (define rkt-type (sham-env-rkt-type s-env name))
  (uintptr->ptr->rkt uintptr rkt-type))
