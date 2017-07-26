#lang racket

(require
  "define.rkt"
  "ctypes.rkt"
  "target-machine.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

(define LLVMByteOrdering (_enum '(LLVMBigEndian LLVMLittleEndian)))
(define-llvm LLVMInitializeX86Target (_fun -> _void))
(define-llvm LLVMInitializeX86TargetInfo (_fun -> _void))
(define-llvm LLVMInitializeX86TargetMC (_fun -> _void))
(define-llvm LLVMInitializeX86AsmPrinter (_fun -> _void))
(define-llvm LLVMInitializeX86AsmParser (_fun -> _void))
(define-llvm LLVMInitializeX86Disassembler (_fun -> _void))

(define-llvm LLVMGetModuleDataLayout (_fun LLVMModuleRef -> LLVMTargetDataRef))
(define-llvm LLVMSetModuleDataLayout (_fun LLVMModuleRef LLVMTargetDataRef -> _void))
(define-llvm LLVMCreateTargetData (_fun _string -> LLVMTargetDataRef))
(define-llvm LLVMDisposeTargetData (_fun LLVMTargetDataRef -> _void))
(define-llvm LLVMAddTargetLibraryInfo (_fun LLVMTargetLibraryInfoRef LLVMPassManagerRef -> _void))
(define-llvm LLVMCopyStringRepOfTargetData (_fun LLVMTargetDataRef -> _string))
(define-llvm LLVMByteOrder (_fun LLVMTargetDataRef -> LLVMByteOrdering))
(define-llvm LLVMPointerSize (_fun LLVMTargetDataRef -> _uint))
(define-llvm LLVMPointerSizeForAS (_fun LLVMTargetDataRef _uint -> _uint))
(define-llvm LLVMIntPtrType (_fun LLVMTargetDataRef -> LLVMTypeRef))
(define-llvm LLVMIntPtrTypeForAS (_fun LLVMTargetDataRef _uint -> LLVMTypeRef))
(define-llvm LLVMIntPtrTypeInContext (_fun LLVMContextRef LLVMTargetDataRef -> LLVMTypeRef))
(define-llvm LLVMIntPtrTypeForASInContext (_fun LLVMContextRef LLVMTargetDataRef _uint -> LLVMTypeRef))
(define-llvm LLVMSizeOfTypeInBits (_fun LLVMTargetDataRef LLVMTypeRef -> _ullong))
(define-llvm LLVMStoreSizeOfType (_fun LLVMTargetDataRef LLVMTypeRef -> _ullong))
(define-llvm LLVMABISizeOfType (_fun LLVMTargetDataRef LLVMTypeRef -> _ullong))
(define-llvm LLVMABIAlignmentOfType (_fun LLVMTargetDataRef LLVMTypeRef -> _uint))
(define-llvm LLVMCallFrameAlignmentOfType (_fun LLVMTargetDataRef LLVMTypeRef -> _uint))
(define-llvm LLVMPreferredAlignmentOfType (_fun LLVMTargetDataRef LLVMTypeRef -> _uint))
(define-llvm LLVMPreferredAlignmentOfGlobal (_fun LLVMTargetDataRef LLVMValueRef -> _uint))
(define-llvm LLVMElementAtOffset (_fun LLVMTargetDataRef LLVMTypeRef _ullong -> _uint))
(define-llvm LLVMOffsetOfElement (_fun LLVMTargetDataRef LLVMTypeRef _uint -> _ullong))