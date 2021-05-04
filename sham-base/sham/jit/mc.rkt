#lang racket

(require (prefix-in llvm- sham/llvm/jit/mc)
         sham/ir/env
         sham/jit/env)

(provide (all-defined-out))

(define (initialize-mcjit s-env)
  (match-define (sham-env s-mod ll-env info) s-env)
  (sham-mcjit-env s-env (llvm-initialize-mcjit ll-env) (make-hash)))

(define (mcjit-function-address mc-env fname)
  (match-define (sham-mcjit-env s-env ll-jit-env value-refs) mc-env)
  (llvm-mcjit-function-address ll-jit-env fname))
