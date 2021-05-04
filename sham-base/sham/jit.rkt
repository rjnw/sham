#lang racket

(require sham/jit/env
         sham/jit/mc
         sham/rkt/types)

(provide (all-from-out sham/jit/env
                       sham/jit/mc)
         (all-defined-out))

(define (initialize-jit senv (jit-type 'mc))
  (case jit-type
    [(llvm-mc mc mc-jit) (initialize-mcjit senv)]
    [else (error 'sham:jit "unsupported jit type ~a" jit-type)]))

(define (jit-lookup-function jit-env fname)
  (match-define (sham-jit-env s-env ll-jit-env value-refs) jit-env)
  (match-define (sham-jit-value _ rkt-value)
    (hash-ref! value-refs fname
               (thunk
                (define fptr (jit-function-address jit-env fname))
                (sham-jit-value fptr (rkt-cast-jit-ptr s-env fname fptr)))))
  rkt-value)

(define (jit-function-address jit-env fname)
  (match jit-env
    [(? sham-mcjit-env?) (mcjit-function-address jit-env fname)]
    [else (error 'sham:jit "unsupported jit type ~a" jit-env)]))
