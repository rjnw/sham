#lang racket
(require ffi/unsafe
         "define.rkt"
         "ctypes.rkt")
(provide (all-defined-out))

(define-llvm LLVMParseIRInContext
  (_fun LLVMContextRef LLVMMemoryBufferRef
        (out-m : (_ptr o LLVMModuleRef)) (out-message : (_ptr o _string))
        -> (fail : LLVMBool)
        -> (vector out-m out-message fail)))
