#lang racket

(require sham/private/md)

(provide (all-defined-out)
         metadata?
         general-ref-key
         general-set-key
         md-ref
         md-ref!
         md-set!)

(define-md module
  [llvm-after-passes
   llvm-before-passes
   jit-external-mappings])

(define-md function
  [llvm-general-attributes
   llvm-return-attributes
   llvm-argument-attributes
   llvm-optimization-pass
   llvm-calling-convention
   jit-rkt-type])

(define-md instruction [llvm-calling-convention])

(define-md struct [llvm-packed])

(define-md type [special-rkt])

(define-md sham-env [rkt-types])
