#lang racket

(require
  "define.rkt"
  "ctypes.rkt")
(require ffi/unsafe)

(provide (all-defined-out))

;;types
(define-llvm LLVMGetTypeKind (_fun LLVMTypeRef -> LLVMTypeKind))
(define-llvm LLVMTypeIsSized (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMGetTypeContext (_fun LLVMTypeRef -> LLVMContextRef))
(define-llvm LLVMDumpType (_fun LLVMTypeRef -> _void))
(define-llvm LLVMPrintTypeToString (_fun LLVMTypeRef -> _string))

;;Integer types
(define-llvm LLVMInt1TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMInt8TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMInt16TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMInt32TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMInt64TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMInt128TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMIntTypeInContext (_fun LLVMContextRef _uint -> LLVMTypeRef))
(define-llvm LLVMInt1Type (_fun -> LLVMTypeRef))
(define-llvm LLVMInt8Type (_fun -> LLVMTypeRef))
(define-llvm LLVMInt16Type (_fun -> LLVMTypeRef))
(define-llvm LLVMInt32Type (_fun -> LLVMTypeRef))
(define-llvm LLVMInt64Type (_fun -> LLVMTypeRef))
(define-llvm LLVMInt128Type (_fun -> LLVMTypeRef))
(define-llvm LLVMIntType (_fun _uint -> LLVMTypeRef))
(define-llvm LLVMGetIntTypeWidth (_fun LLVMTypeRef -> unsigned))

;;floating point types
(define-llvm LLVMHalfTypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMFloatTypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMDoubleTypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMX86FP80TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMFP128TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMPPCFP128TypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMHalfType (_fun -> LLVMTypeRef))
(define-llvm LLVMFloatType (_fun -> LLVMTypeRef))
(define-llvm LLVMDoubleType (_fun -> LLVMTypeRef))
(define-llvm LLVMX86FP80Type (_fun -> LLVMTypeRef))
(define-llvm LLVMFP128Type (_fun -> LLVMTypeRef))
(define-llvm LLVMPPCFP128Type (_fun -> LLVMTypeRef))

;;function types
(define-llvm LLVMFunctionType (_fun LLVMTypeRef (pointer-to LLVMTypeRef) _uint LLVMBool -> LLVMTypeRef))
(define-llvm LLVMIsFunctionVarArg (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMGetReturnType (_fun LLVMTypeRef -> LLVMTypeRef))
(define-llvm LLVMCountParamTypes (_fun LLVMTypeRef -> unsigned))
(define-llvm LLVMGetParamTypes (_fun LLVMTypeRef (pointer-to LLVMTypeRef) -> _void))

;;structure types
(define-llvm LLVMStructTypeInContext (_fun LLVMContextRef (pointer-to LLVMTypeRef) _uint LLVMBool -> LLVMTypeRef))
(define-llvm LLVMStructType (_fun (pointer-to LLVMTypeRef) _uint LLVMBool -> LLVMTypeRef))
(define-llvm LLVMStructCreateNamed (_fun LLVMContextRef _string -> LLVMTypeRef))
(define-llvm LLVMGetStructName (_fun LLVMTypeRef -> _string))
(define-llvm LLVMStructSetBody (_fun LLVMTypeRef (pointer-to LLVMTypeRef) _uint LLVMBool -> _void))
(define-llvm LLVMCountStructElementTypes (_fun LLVMTypeRef -> unsigned))
(define-llvm LLVMGetStructElementTypes (_fun LLVMTypeRef (pointer-to LLVMTypeRef) -> _void))
(define-llvm LLVMStructGetTypeAtIndex (_fun LLVMTypeRef _uint -> LLVMTypeRef))
(define-llvm LLVMIsPackedStruct (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMIsOpaqueStruct (_fun LLVMTypeRef -> LLVMBool))

;;sequential types
(define-llvm LLVMGetElementType (_fun LLVMTypeRef -> LLVMTypeRef))
(define-llvm LLVMArrayType (_fun LLVMTypeRef _uint -> LLVMTypeRef))
(define-llvm LLVMGetArrayLength (_fun LLVMTypeRef -> unsigned))
(define-llvm LLVMPointerType (_fun LLVMTypeRef _uint -> LLVMTypeRef))
(define-llvm LLVMGetPointerAddressSpace (_fun LLVMTypeRef -> unsigned))
(define-llvm LLVMVectorType (_fun LLVMTypeRef _uint -> LLVMTypeRef))
(define-llvm LLVMGetVectorSize (_fun LLVMTypeRef -> unsigned))

;;other types
(define-llvm LLVMVoidTypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMLabelTypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMX86MMXTypeInContext (_fun LLVMContextRef -> LLVMTypeRef))
(define-llvm LLVMVoidType (_fun -> LLVMTypeRef))
(define-llvm LLVMLabelType (_fun -> LLVMTypeRef))
(define-llvm LLVMX86MMXType (_fun -> LLVMTypeRef))
