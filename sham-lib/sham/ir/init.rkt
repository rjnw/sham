#lang racket
(require sham/llvm/ffi/all
         sham/llvm/ffi/ctypes
         sham/env/base)

(require "internals.rkt"
         "types.rkt")

(provide (all-defined-out))

(define (create-context) (LLVMContextCreate))
(define global-context (make-parameter (LLVMGetGlobalContext)))
(define context? LLVMContextRef?)

(define (create-initial-environment context)
  (register-llvm-internals (register-initial-types (empty-env) context) context))
(define full-data-layout
  (string-join
   '("e"
     "m:e"
     "p:64:64:64"
     "i1:8:8"
     "i8:8:8"
     "i16:16:16"
     "i32:32:32"
     "i64:64:64"
     ;;integer types alignment
     "f32:32:32"
     "f64:64:64"
     "f128:128:128" ;;floating types alignment
     "v64:64:64"
     "v128:128:128" ;; vector type alignment
     "a:0:64"
     "s0:64:64"
     "n8:16:32:64" ;; a aggregate alignment
     "S128"        ;stack alignment
     )
   "-"))
