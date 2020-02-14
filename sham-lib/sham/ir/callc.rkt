#lang racket

(require sham/llvm
         ffi/unsafe)
(provide (all-defined-out))

(define (llvm-calling-convention->uint call-conv)
  (cast (string->symbol (format "LLVM~aCallConv" call-conv))
        LLVMCallConv _uint))
(define (uint->llvm-calling-convention i)
  (cast i _uint LLVMCallConv))

(define (set-instruction-call-conv! cs conv)
  (LLVMSetInstructionCallConv cs (llvm-calling-convention->uint conv)))
(define (get-instruction-call-conv cs)
  (uint->llvm-calling-convention (LLVMGetInstructionCallConv cs)))

(define (set-function-call-conv! f conv)
  (LLVMSetFunctionCallConv f (llvm-calling-convention->uint conv)))
(define (get-function-call-conv f)
  (uint->llvm-calling-convention (LLVMGetFunctionCallConv f)))
