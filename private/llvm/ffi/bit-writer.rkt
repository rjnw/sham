#lang racket

(require
  "define.rkt"
  "ctypes.rkt"
  ffi/unsafe)

(provide (all-defined-out))

(define-llvm LLVMWriteBitcodeToFile (_fun LLVMModuleRef _string -> _int))
(define-llvm LLVMWriteBitcodeToFD (_fun LLVMModuleRef _int _int _int -> _int))
(define-llvm LLVMWriteBitcodeToFileHandle (_fun LLVMModuleRef _int -> _int))
(define-llvm LLVMWriteBitcodeToMemoryBuffer (_fun LLVMModuleRef -> LLVMMemoryBufferRef))
