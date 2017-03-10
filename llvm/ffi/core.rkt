#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;; modules
(define-llvm LLVMModuleCreateWithName (_fun _string -> LLVMModuleRef))
(define-llvm LLVMModuleCreateWithNameInContext (_fun _string LLVMContextRef -> LLVMModuleRef))
(define-llvm LLVMCloneModule (_fun LLVMModuleRef -> LLVMModuleRef))
(define-llvm LLVMDisposeModule (_fun LLVMModuleRef -> _void))
(define-llvm LLVMGetDataLayout (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetDataLayout (_fun LLVMModuleRef _string -> _void))
(define-llvm LLVMGetTarget (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetTarget (_fun LLVMModuleRef _string -> _void))
(define-llvm LLVMDumpModule (_fun LLVMModuleRef -> _void))
(define-llvm LLVMPrintModuleToFile (_fun LLVMModuleRef _string _string -> LLVMBool))
(define-llvm LLVMPrintModuleToString (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetModuleInlineAsm (_fun LLVMModuleRef _string -> _void))
(define-llvm LLVMGetModuleContext (_fun LLVMModuleRef -> LLVMContextRef))
(define-llvm LLVMGetTypeByName (_fun LLVMModuleRef _string -> LLVMTypeRef))
(define-llvm LLVMGetNamedMetadataNumOperands (_fun LLVMModuleRef _string -> _uint))
(define-llvm LLVMGetNamedMetadataOperands (_fun LLVMModuleRef _string (pointer-to LLVMValueRef) -> _void))
(define-llvm LLVMAddNamedMetadataOperand (_fun LLVMModuleRef _string LLVMValueRef -> _void))
(define-llvm LLVMAddFunction (_fun LLVMModuleRef _string LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMGetNamedFunction (_fun LLVMModuleRef _string -> LLVMValueRef))
(define-llvm LLVMGetFirstFunction (_fun LLVMModuleRef -> LLVMValueRef))
(define-llvm LLVMGetLastFunction (_fun LLVMModuleRef -> LLVMValueRef))
(define-llvm LLVMGetNextFunction (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetPreviousFunction (_fun LLVMValueRef -> LLVMValueRef))



;; basic blocks
(define-llvm LLVMBasicBlockAsValue (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMValueIsBasicBlock (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMValueAsBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetBasicBlockParent (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMGetBasicBlockTerminator (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMCountBasicBlocks (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetBasicBlocks (_fun LLVMValueRef (pointer-to LLVMBasicBlockRef) -> _void))
(define-llvm LLVMGetFirstBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetLastBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetNextBasicBlock (_fun LLVMBasicBlockRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetPreviousBasicBlock (_fun LLVMBasicBlockRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetEntryBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMAppendBasicBlockInContext (_fun LLVMContextRef LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMAppendBasicBlock (_fun LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlockInContext (_fun LLVMContextRef LLVMBasicBlockRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlock (_fun LLVMBasicBlockRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMDeleteBasicBlock (_fun LLVMBasicBlockRef -> _void))
(define-llvm LLVMRemoveBasicBlockFromParent (_fun LLVMBasicBlockRef -> _void))
(define-llvm LLVMMoveBasicBlockBefore (_fun LLVMBasicBlockRef LLVMBasicBlockRef -> _void))
(define-llvm LLVMMoveBasicBlockAfter (_fun LLVMBasicBlockRef LLVMBasicBlockRef -> _void))
(define-llvm LLVMGetFirstInstruction (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMGetLastInstruction (_fun LLVMBasicBlockRef -> LLVMValueRef))


;; metadata
(define-llvm LLVMMDStringInContext (_fun LLVMContextRef _string _uint -> LLVMValueRef))
(define-llvm LLVMMDString (_fun _string _uint -> LLVMValueRef))
(define-llvm LLVMMDNodeInContext (_fun LLVMContextRef (pointer-to LLVMValueRef) _uint -> LLVMValueRef))
(define-llvm LLVMMDNode (_fun (pointer-to LLVMValueRef) _uint -> LLVMValueRef))
(define-llvm LLVMGetMDString (_fun LLVMValueRef (pointer-to unsigned) -> _string))
(define-llvm LLVMGetMDNodeNumOperands (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetMDNodeOperands (_fun LLVMValueRef (pointer-to LLVMValueRef) -> _void))

;; contexts
(define LLVMDiagnosticHandler (_fun LLVMDiagnosticInfoRef _pointer -> _void))
(define LLVMYieldCallback (_fun LLVMContextRef _pointer -> _void))

(define-llvm LLVMContextCreate (_fun -> LLVMContextRef))
(define-llvm LLVMGetGlobalContext (_fun -> LLVMContextRef))
(define-llvm LLVMContextSetDiagnosticHandler (_fun LLVMContextRef LLVMDiagnosticHandler (pointer-to _void) -> _void))
(define-llvm LLVMContextSetYieldCallback (_fun LLVMContextRef LLVMYieldCallback (pointer-to _void) -> _void))
(define-llvm LLVMContextDispose (_fun LLVMContextRef -> _void))
(define-llvm LLVMGetDiagInfoDescription (_fun LLVMDiagnosticInfoRef -> _string))
(define-llvm LLVMGetDiagInfoSeverity (_fun LLVMDiagnosticInfoRef -> LLVMDiagnosticSeverity))
(define-llvm LLVMGetMDKindIDInContext (_fun LLVMContextRef _string _uint -> unsigned))
