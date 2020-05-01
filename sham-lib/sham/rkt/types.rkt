#lang racket

(require sham/llvm/ir/ast
         sham/ir/ast/core

         sham/ir/env)
(require ffi/unsafe)
(provide sham-env-rkt-type)

(define generic-key '#%generic-rkt-types)
(define special-key '#%special-rkt-types)

(define (to-llvm-type sym)
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

(define (build-generic-rkt-types s-env)
  (match-define (sham-env s-mod ll-env s-info) s-env)
  (match-define (sham-module s-ast ll-ast ext) s-mod)
  (match-define (sham:def:module m-info mod-id defs) s-ast)
  (define def-map (for/hash ([d defs])
                    (values (if (sham:def? d) (sham:def-id d) (llvm:def-id d)) d)))
  (for/hash ([d defs]
             #:when (sham:def:function? d))
    (match-define (sham:def:function f-info f-name t-ast f-body) d)
    (define (rec t)
      (match t
        [(llvm:ast:type:ref _ to)
         (cond
           [(to-llvm-type to) => identity]
           [(hash-ref def-map to #f)
            =>
            (λ (d)
              (match d
                [(llvm:def:type _ _ t) (rec t)]
                [(sham:def:struct _ _ _ _) #f]))])]
        [(llvm:ast:type:pointer _ _) _pointer]
        [(llvm:ast:type:array _ _ _) _pointer]
        [else #f]))
    (values f-name (rec t-ast))))

(define (build-special-rkt-types s-env) (make-hash)) ;TODO

(define (populate-rkt-types! s-env)
  (match-define (sham-env s-mod ll-env s-info) s-env)
  (unless (hash-has-key? s-info generic-key)
    (hash-set! s-info generic-key (build-generic-rkt-types s-env)))
  (unless (hash-has-key? s-info special-key)
    (hash-set! s-info special-key (build-special-rkt-types s-env))))

(define (sham-env-rkt-type s-env fname)
  (match-define (sham-env s-mod ll-env s-info) s-env)
  (populate-rkt-types! s-env)
  (define special-types (hash-ref s-info special-key))
  (cond
    [(hash-ref special-types fname #f) => identity]
    [else (hash-ref (hash-ref s-info generic-key fname))]))
