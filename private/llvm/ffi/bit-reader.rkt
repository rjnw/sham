#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

(define-llvm LLVMParseBitcode (_fun LLVMMemoryBufferRef (pointer-to LLVMModuleRef) _string -> LLVMBool))
(define-llvm LLVMParseBitcode2 (_fun LLVMMemoryBufferRef (pointer-to LLVMModuleRef) -> LLVMBool))
(define-llvm LLVMParseBitcodeInContext (_fun LLVMContextRef LLVMMemoryBufferRef (pointer-to LLVMModuleRef) _string -> LLVMBool))
(define-llvm LLVMParseBitcodeInContext2 (_fun LLVMContextRef LLVMMemoryBufferRef (pointer-to LLVMModuleRef) -> LLVMBool))
(define-llvm LLVMGetBitcodeModuleInContext (_fun LLVMContextRef LLVMMemoryBufferRef (pointer-to LLVMModuleRef) _string -> LLVMBool))
(define-llvm LLVMGetBitcodeModuleInContext2 (_fun LLVMContextRef LLVMMemoryBufferRef (pointer-to LLVMModuleRef) -> LLVMBool))
(define-llvm LLVMGetBitcodeModule (_fun LLVMMemoryBufferRef (pointer-to LLVMModuleRef) _string -> LLVMBool))
(define-llvm LLVMGetBitcodeModule2 (_fun LLVMMemoryBufferRef (pointer-to LLVMModuleRef) -> LLVMBool))
