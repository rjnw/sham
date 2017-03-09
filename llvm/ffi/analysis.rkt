#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


; LLVMCAnalysis
; http://llvm.org/docs/doxygen/html/group__LLVMCAnalysis.html

(define LLVMVerifierFailureAction
  (_enum '(LLVMAbortProcessAction LLVMPrintMessageAction LLVMReturnStatusAction)))

(define-llvm LLVMVerifyModule (_fun LLVMModuleRef LLVMVerifierFailureAction _string -> LLVMBool))
(define-llvm LLVMVerifyFunction (_fun LLVMValueRef LLVMVerifierFailureAction -> LLVMBool))
(define-llvm LLVMViewFunctionCFG (_fun LLVMValueRef -> _void))
(define-llvm LLVMViewFunctionCFGOnly (_fun LLVMValueRef -> _void))
