#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

(define-llvm-types
  LLVMTargetDataRef
  LLVMTargetLibraryInfoRef)

(define LLVMByteOrdering (_enum '(LLVMBigEndian LLVMLittleEndian)))
;; (define-llvm LLVMInitializeAllTargetInfos (_fun -> _void))
;; (define-llvm LLVMInitializeAllTargets (_fun -> _void))
;; (define-llvm LLVMInitializeAllTargetMCs (_fun -> _void))
;; (define-llvm LLVMInitializeAllAsmPrinters (_fun -> _void))
;; (define-llvm LLVMInitializeAllAsmParsers (_fun -> _void))
;; (define-llvm LLVMInitializeAllDisassemblers (_fun -> _void))
;; (define-llvm LLVMInitializeNativeTarget (_fun -> LLVMBool))
;; (define-llvm LLVMInitializeNativeAsmParser (_fun -> LLVMBool))
;; (define-llvm LLVMInitializeNativeAsmPrinter (_fun -> LLVMBool))
;; (define-llvm LLVMInitializeNativeDisassembler (_fun -> LLVMBool))
(define-llvm LLVMCreateTargetData (_fun _string -> LLVMTargetDataRef))
;; (define-llvm LLVMAddTargetData (_fun LLVMTargetDataRef LLVMPassManagerRef -> _void))
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
(define-llvm LLVMDisposeTargetData (_fun LLVMTargetDataRef -> _void))
