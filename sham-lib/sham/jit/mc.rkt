#lang racket

(require sham/llvm/ir/env
         sham/llvm/jit/mc
         sham/llvm/jit/env
         sham/ir/env
         sham/jit/env
         sham/rkt/types)

(require ffi/unsafe)

(provide (all-defined-out))

(define (sham-initialize-mcjit s-env)
  (match-define (sham-env s-mod ll-env info) s-env)
  (sham-mcjit-env s-env (llvm-initialize-mcjit ll-env) (make-hash)))

(define (sham-mcjit-lookup-function mc-env fname)
  (match-define (sham-mcjit-env s-env ll-jit-env value-refs) mc-env)
  (match-define (sham-jit-value _ rkt-value)
    (hash-ref! value-refs fname
               (thunk
                (define rkt-type (sham-env-rkt-type s-env fname))
                (define fptr (mcjit-function-address ll-jit-env fname))
                (sham-jit-value fptr (cast (cast fptr _uintptr _pointer) _pointer rkt-type)))))
  rkt-value)
