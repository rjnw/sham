#lang racket

(require sham/llvm/ffi/all
         sham/llvm/ffi/ctypes)

(provide (all-defined-out))

(define (create-llvm-context) (LLVMContextCreate))
(define global-llvm-context (make-parameter (LLVMGetGlobalContext)))
(define llvm-context? LLVMContextRef?)

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
