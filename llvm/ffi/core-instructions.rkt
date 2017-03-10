#lang racket

(require
  "define.rkt"
  "ctypes.rkt")
(require ffi/unsafe)

(provide (all-defined-out))

;;instructions
(define-llvm LLVMHasMetadata (_fun LLVMValueRef -> _int))
(define-llvm LLVMGetMetadata (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMSetMetadata (_fun LLVMValueRef _uint LLVMValueRef -> _void))
(define-llvm LLVMGetInstructionParent (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetNextInstruction (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetPreviousInstruction (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMInstructionEraseFromParent (_fun LLVMValueRef -> _void))
(define-llvm LLVMGetInstructionOpcode (_fun LLVMValueRef -> LLVMOpcode))
(define-llvm LLVMGetICmpPredicate (_fun LLVMValueRef -> LLVMIntPredicate))
(define-llvm LLVMGetFCmpPredicate (_fun LLVMValueRef -> LLVMRealPredicate))
(define-llvm LLVMInstructionClone (_fun LLVMValueRef -> LLVMValueRef))

;;call sites and invocations
(define-llvm LLVMSetInstructionCallConv (_fun LLVMValueRef _uint -> _void))
(define-llvm LLVMGetInstructionCallConv (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMAddInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))
(define-llvm LLVMRemoveInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))
(define-llvm LLVMSetInstrParamAlignment (_fun LLVMValueRef _uint _uint -> _void))
(define-llvm LLVMIsTailCall (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetTailCall (_fun LLVMValueRef LLVMBool -> _void))

;;terminatores
(define-llvm LLVMGetNumSuccessors (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetSuccessor (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))
(define-llvm LLVMSetSuccessor (_fun LLVMValueRef _uint LLVMBasicBlockRef -> _void))
(define-llvm LLVMIsConditional (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMGetCondition (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMSetCondition (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMGetSwitchDefaultDest (_fun LLVMValueRef -> LLVMBasicBlockRef))

;;phi nodes
(define-llvm LLVMAddIncoming (_fun LLVMValueRef (pointer-to LLVMValueRef) (pointer-to LLVMBasicBlockRef) _uint -> _void))
(define-llvm LLVMCountIncoming (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetIncomingValue (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMGetIncomingBlock (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))
