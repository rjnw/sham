#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)
(require ffi/unsafe/alloc)
(provide (all-defined-out))

(define LLVMOpcode
  (_enum
   '(LLVMRet            = 1
     LLVMBr             = 2
     LLVMSwitch         = 3
     LLVMIndirectBr     = 4
     LLVMInvoke         = 5
     LLVMUnreachable    = 7
     LLVMAdd            = 8
     LLVMFAdd           = 9
     LLVMSub            = 10
     LLVMFSub           = 11
     LLVMMul            = 12
     LLVMFMul           = 13
     LLVMUDiv           = 14
     LLVMSDiv           = 15
     LLVMFDiv           = 16
     LLVMURem           = 17
     LLVMSRem           = 18
     LLVMFRem           = 19
     LLVMShl            = 20
     LLVMLShr           = 21
     LLVMAShr           = 22
     LLVMAnd            = 23
     LLVMOr             = 24
     LLVMXor            = 25
     LLVMAlloca         = 26
     LLVMLoad           = 27
     LLVMStore          = 28
     LLVMGetElementPtr  = 29
     LLVMTrunc          = 30
     LLVMZExt           = 31
     LLVMSExt           = 32
     LLVMFPToUI         = 33
     LLVMFPToSI         = 34
     LLVMUIToFP         = 35
     LLVMSIToFP         = 36
     LLVMFPTrunc        = 37
     LLVMFPExt          = 38
     LLVMPtrToInt       = 39
     LLVMIntToPtr       = 40
     LLVMBitCast        = 41
     LLVMAddrSpaceCast  = 60
     LLVMICmp           = 42
     LLVMFCmp           = 43
     LLVMPHI            = 44
     LLVMCall           = 45
     LLVMSelect         = 46
     LLVMUserOp1        = 47
     LLVMUserOp2        = 48
     LLVMVAArg          = 49
     LLVMExtractElement = 50
     LLVMInsertElement  = 51
     LLVMShuffleVector  = 52
     LLVMExtractValue   = 53
     LLVMInsertValue    = 54
     LLVMFence          = 55
     LLVMAtomicCmpXchg  = 56
     LLVMAtomicRMW      = 57
     LLVMResume         = 58
     LLVMLandingPad     = 59
     LLVMCleanupRet     = 61
     LLVMCatchRet       = 62
     LLVMCatchPad       = 63
     LLVMCleanupPad     = 64
     LLVMCatchSwitch    = 65)))

(define LLVMTypeKind
  (_enum
   '(LLVMVoidTypeKind
     LLVMHalfTypeKind
     LLVMFloatTypeKind
     LLVMDoubleTypeKind
     LLVMX86_FP80TypeKind
     LLVMFP128TypeKind
     LLVMPPC_FP128TypeKind
     LLVMLabelTypeKind
     LLVMIntegerTypeKind
     LLVMFunctionTypeKind
     LLVMStructTypeKind
     LLVMArrayTypeKind
     LLVMPointerTypeKind
     LLVMVectorTypeKind
     LLVMMetadataTypeKind
     LLVMX86_MMXTypeKind
     LLVMTokenTypeKind)))

(define LLVMLinkage
  (_enum
   '(LLVMExternalLinkage
     LLVMAvailableExternallyLinkage
     LLVMLinkOnceAnyLinkage
     LLVMLinkOnceODRLinkage
     LLVMLinkOnceODRAutoHideLinkage
     LLVMWeakAnyLinkage
     LLVMWeakODRLinkage
     LLVMAppendingLinkage
     LLVMInternalLinkage
     LLVMPrivateLinkage
     LLVMDLLImportLinkage
     LLVMDLLExportLinkage
     LLVMExternalWeakLinkage
     LLVMGhostLinkage
     LLVMCommonLinkage
     LLVMLinkerPrivateLinkage
     LLVMLinkerPrivateWeakLinkage)))

(define LLVMVisibility
  (_enum
   '(LLVMDefaultVisibility
     LLVMHiddenVisibility
     LLVMProtectedVisibility)))

(define LLVMDLLStorageClass
  (_enum
   '(LLVMDefaultStorageClass   = 0
     LLVMDLLImportStorageClass = 1
     LLVMDLLExportStorageClass = 2)))

(define LLVMCallConv
  (_enum
   '(
     LLVMCCallConv             = 0
     LLVMFastCallConv          = 8
     LLVMColdCallConv          = 9
     LLVMGHCCallConv           = 10
     LLVMHiPECallConv          = 11
     LLVMWebKitJSCallConv      = 12
     LLVMAnyRegCallConv        = 13
     LLVMPreserveMostCallConv  = 14
     LLVMPreserveAllCallConv   = 15
     LLVMSwiftCallConv         = 16
     LLVMCXXFASTTLSCallConv    = 17
     LLVMX86StdcallCallConv    = 64
     LLVMX86FastcallCallConv   = 65
     LLVMARMAPCSCallConv       = 66
     LLVMARMAAPCSCallConv      = 67
     LLVMARMAAPCSVFPCallConv   = 68
     LLVMMSP430INTRCallConv    = 69
     LLVMX86ThisCallCallConv   = 70
     LLVMPTXKernelCallConv     = 71
     LLVMPTXDeviceCallConv     = 72
     LLVMSPIRFUNCCallConv      = 75
     LLVMSPIRKERNELCallConv    = 76
     LLVMIntelOCLBICallConv    = 77
     LLVMX8664SysVCallConv     = 78
     LLVMWin64CallConv         = 79
     LLVMX86VectorCallCallConv = 80
     LLVMHHVMCallConv          = 81
     LLVMHHVMCCallConv         = 82
     LLVMX86INTRCallConv       = 83
     LLVMAVRINTRCallConv       = 84
     LLVMAVRSIGNALCallConv     = 85
     LLVMAVRBUILTINCallConv    = 86
     LLVMAMDGPUVSCallConv      = 87
     LLVMAMDGPUGSCallConv      = 88
     LLVMAMDGPUPSCallConv      = 89
     LLVMAMDGPUCSCallConv      = 90
     LLVMAMDGPUKERNELCallConv  = 91
     LLVMX86RegCallCallConv    = 92
     LLVMAMDGPUHSCallConv      = 93
     LLVMMSP430BUILTINCallConv = 94
     LLVMAMDGPULSCallConv      = 95
     LLVMAMDGPUESCallConv      = 96)))

(define LLVMValueKind
  (_enum
   '(LLVMArgumentValueKind
     LLVMBasicBlockValueKind
     LLVMMemoryUseValueKind
     LLVMMemoryDefValueKind
     LLVMMemoryPhiValueKind

     LLVMFunctionValueKind
     LLVMGlobalAliasValueKind
     LLVMGlobalIFuncValueKind
     LLVMGlobalVariableValueKind
     LLVMBlockAddressValueKind
     LLVMConstantExprValueKind
     LLVMConstantArrayValueKind
     LLVMConstantStructValueKind
     LLVMConstantVectorValueKind

     LLVMUndefValueValueKind
     LLVMConstantAggregateZeroValueKind
     LLVMConstantDataArrayValueKind
     LLVMConstantDataVectorValueKind
     LLVMConstantIntValueKind
     LLVMConstantFPValueKind
     LLVMConstantPointerNullValueKind
     LLVMConstantTokenNoneValueKind

     LLVMMetadataAsValueValueKind
     LLVMInlineAsmValueKind

     LLVMInstructionValueKind)))

(define LLVMIntPredicate
  (_enum
   '(LLVMIntEQ = 32
     LLVMIntNE
     LLVMIntUGT
     LLVMIntUGE
     LLVMIntULT
     LLVMIntULE
     LLVMIntSGT
     LLVMIntSGE
     LLVMIntSLT
     LLVMIntSLE)))

(define LLVMRealPredicate
  (_enum
   '(LLVMRealPredicateFalse
     LLVMRealOEQ
     LLVMRealOGT
     LLVMRealOGE
     LLVMRealOLT
     LLVMRealOLE
     LLVMRealONE
     LLVMRealORD
     LLVMRealUNO
     LLVMRealUEQ
     LLVMRealUGT
     LLVMRealUGE
     LLVMRealULT
     LLVMRealULE
     LLVMRealUNE
     LLVMRealPredicateTrue)))

(define LLVMLandingPadClauseTy
  (_enum '(LLVMLandingPadCatch
           LLVMLandingPadFilter)))

(define LLVMThreadLocalMode
  (_enum
   '(LLVMNotThreadLocal = 0
     LLVMGeneralDynamicTLSModel
     LLVMLocalDynamicTLSModel
     LLVMInitialExecTLSModel
     LLVMLocalExecTLSModel)))

(define LLVMAtomicOrdering
  (_enum
   '(LLVMAtomicOrderingNotAtomic              = 0
     LLVMAtomicOrderingUnordered              = 1
     LLVMAtomicOrderingMonotonic              = 2
     LLVMAtomicOrderingAcquire                = 4
     LLVMAtomicOrderingRelease                = 5
     LLVMAtomicOrderingAcquireRelease         = 6
     LLVMAtomicOrderingSequentiallyConsistent = 7)))

(define LLVMAtomicRMWBinOp
  (_enum
   '(LLVMAtomicRMWBinOpXchg
     LLVMAtomicRMWBinOpAdd
     LLVMAtomicRMWBinOpSub
     LLVMAtomicRMWBinOpAnd
     LLVMAtomicRMWBinOpNand
     LLVMAtomicRMWBinOpOr
     LLVMAtomicRMWBinOpXor
     LLVMAtomicRMWBinOpMax
     LLVMAtomicRMWBinOpMin
     LLVMAtomicRMWBinOpUMax
     LLVMAtomicRMWBinOpUMin)))

(define LLVMDiagnosticSeverity
  (_enum
   '(LLVMDSError
     LLVMDSWarning
     LLVMDSRemark
     LLVMDSNote)))

(define LLVMAttributeIndex unsigned)


(define-llvm LLVMShutdown (_fun -> _void))




;; LLVMCCoreContext
#|
* Contexts are execution states for the core LLVM IR system.
*
* Most types are tied to a context instance. Multiple contexts can
* exist simultaneously. A single context is not thread safe. However,
* different contexts can execute on different threads simultaneously.
|#
(define LLVMDiagnosticHandler (_fun LLVMDiagnosticInfoRef _pointer -> _void))
(define LLVMYieldCallback (_fun LLVMContextRef _pointer -> _void))

(define-llvm LLVMContextDispose (_fun LLVMContextRef -> _void)
  #:wrap (deallocator))
;; Create a new context, racket ffi will automatically call
;; dispose using gc

(define-llvm LLVMContextCreate (_fun -> LLVMContextRef)
  #:wrap (allocator LLVMContextDispose))

(define-llvm LLVMGetGlobalContext (_fun -> LLVMContextRef))
(define-llvm LLVMContextSetDiagnosticHandler (_fun LLVMContextRef LLVMDiagnosticHandler (pointer-to _void) -> _void))
(define-llvm LLVMContextSetYieldCallback (_fun LLVMContextRef LLVMYieldCallback (pointer-to _void) -> _void))

(define-llvm LLVMDisposeMessage (_fun _pointer -> _void)
  #:wrap (deallocator))
(define-llvm LLVMGetDiagInfoDescription (_fun LLVMDiagnosticInfoRef -> _pointer)
  #:wrap (allocator LLVMDisposeMessage)) ;; TODO _string changes pointer figure out how to dispose original
(define-llvm LLVMGetDiagInfoSeverity (_fun LLVMDiagnosticInfoRef -> LLVMDiagnosticSeverity))

(define-llvm LLVMCreateMessage (_fun _string -> _pointer)
  #:wrap (allocator LLVMDisposeMessage))


(define-llvm LLVMGetMDKindIDInContext (_fun LLVMContextRef _string _uint -> unsigned))
(define-llvm LLVMGetMDKindID (_fun _string _uint -> unsigned))

;; Attributes
#|
 Return an unique id given the name of a enum attribute,
 or 0 if no attribute by that name exists.

 See http://llvm.org/docs/LangRef.html#parameter-attributes
 and http://llvm.org/docs/LangRef.html#function-attributes
 for the list of available attributes.
|#
(define-llvm LLVMGetEnumAttributeKindForName
  (_fun (name : _string) (_size = (string-length name)) -> _uint))
(define-llvm LLVMGetLastEnumAttributeKind (_fun -> _void))

;; Create an enum attribute
(define-llvm LLVMCreateEnumAttribute
  (_fun LLVMContextRef (kindid : _uint) (value : _uint64) -> LLVMAttributeRef))

;; Get the unique id corresponding to the enum attribute  passed as argument.
(define-llvm LLVMGetEnumAttributeKind (_fun LLVMAttributeRef -> _uint))
;; Get the enum attribute's value. 0 is returned if none exists.
(define-llvm LLVMGetEnumAttributeValue (_fun LLVMAttributeRef -> _uint64))

(define-llvm LLVMCreateStringAttribute
  (_fun LLVMContextRef
        (K : _string) (_uint = (string-length K))
        (V : _string) (_uint = (string-length V))
        -> LLVMAttributeRef))
(define-llvm LLVMGetStringAttributeKind (_fun LLVMAttributeRef _uint -> _string))
(define-llvm LLVMGetStringAttributeValue (_fun LLVMAttributeRef _uint -> _string))

(define-llvm LLVMIsEnumAttribute (_fun LLVMAttributeRef -> LLVMBool))
(define-llvm LLVMIsStringAttribute (_fun LLVMAttributeRef -> LLVMBool))

;; LLVMCCoreModules
#|
 * Modules represent the top-level structure in an LLVM program. An LLVM
 * module is effectively a translation unit or a collection of
 * translation units merged together.
 |#

(define-llvm LLVMDisposeModule (_fun LLVMModuleRef -> _void)
  #:wrap (deallocator))
(define-llvm LLVMModuleCreateWithName (_fun _string -> LLVMModuleRef)
  #:wrap (allocator LLVMDisposeModule))
(define-llvm LLVMModuleCreateWithNameInContext (_fun _string LLVMContextRef -> LLVMModuleRef)
  #:wrap (allocator LLVMDisposeModule))
(define-llvm LLVMCloneModule (_fun LLVMModuleRef -> LLVMModuleRef))

(define-llvm LLVMGetModuleIdentifier
  (_fun LLVMModuleRef (len : (_ptr o _size)) -> (ident : _bytes) ->
        (vector ident len)))

(define-llvm LLVMSetModuleIdentifier
  (_fun LLVMModuleRef (ident : _string) (len : _size = (string-length ident)) -> _void))

(define-llvm LLVMGetDataLayoutStr (_fun LLVMModuleRef -> _string))
(define-llvm LLVMGetDataLayout (_fun LLVMModuleRef -> _string)) ;;Deprecated
(define-llvm LLVMSetDataLayout (_fun LLVMModuleRef _string -> _void))
(define-llvm LLVMGetTarget (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetTarget (_fun LLVMModuleRef _string -> _void))
(define-llvm LLVMDumpModule (_fun LLVMModuleRef -> _void))
;; Error message needs to be disposed with LLVMDisposeMessage
;; This didn't seem to work
(define-llvm LLVMPrintModuleToFile
  (_fun LLVMModuleRef _string (err-msg : (_ptr o _string))
        -> (success? : LLVMBool)
        -> (vector success? err-msg)))

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

;;LLVMCCoreTypes
#|
 * Types represent the type of a value.
 *
 * Types are associated with a context instance. The context internally
 * deduplicates types so there is only 1 instance of a specific type
 * alive at a time. In other words, a unique type is shared among all
 * consumers within a context.
 *
 * A Type in the C API corresponds to llvm::Type.
 *
 * Types have the following hierarchy:
 *
 *   types:
 *     integer type
 *     real type
 *     function type
 *     sequence types:
 *       array type
 *       pointer type
 *       vector type
 *     void type
 *     label type
 *     opaque type
 *
|#
;;types
(define-llvm LLVMGetTypeKind (_fun LLVMTypeRef -> LLVMTypeKind))
(define-llvm LLVMTypeIsSized (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMGetTypeContext (_fun LLVMTypeRef -> LLVMContextRef))
;(define-llvm LLVMDumpType (_fun LLVMTypeRef -> _void))
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
(define-llvm LLVMFunctionType (_fun LLVMTypeRef
                                    [arg-types : (_list i LLVMTypeRef)]
                                    [num-args : _uint = (length arg-types)] LLVMBool -> LLVMTypeRef))
(define-llvm LLVMIsFunctionVarArg (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMGetReturnType (_fun LLVMTypeRef -> LLVMTypeRef))
(define-llvm LLVMCountParamTypes (_fun LLVMTypeRef -> unsigned))
(define-llvm LLVMGetParamTypes (_fun LLVMTypeRef (pointer-to LLVMTypeRef) -> _void))

;;structure types
(define-llvm LLVMStructTypeInContext (_fun LLVMContextRef (pointer-to LLVMTypeRef) _uint LLVMBool -> LLVMTypeRef))
(define-llvm LLVMStructType
  (_fun [elements : (_list i LLVMTypeRef)]
        [num-elements : _uint = (length elements)]
        LLVMBool
        -> LLVMTypeRef))
(define-llvm LLVMStructCreateNamed (_fun LLVMContextRef _string -> LLVMTypeRef))
(define-llvm LLVMGetStructName (_fun LLVMTypeRef -> _string))
(define-llvm LLVMStructSetBody (_fun LLVMTypeRef
                                     [elements : (_list i LLVMTypeRef)]
                                     [num_elements : _uint = (length elements)]
                                     LLVMBool
                                     -> _void))
(define-llvm LLVMCountStructElementTypes (_fun LLVMTypeRef -> unsigned))
(define-llvm LLVMGetStructElementTypes
  (_fun (type : LLVMTypeRef) (fields : (_list o LLVMTypeRef (LLVMCountStructElementTypes type)))
        -> _void -> fields))
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

;; LLVMCCoreValueGeneral
#|
 * Functions in this section work on all LLVMValueRef instances,
 * regardless of their sub-type. They correspond to functions available
 * on llvm::Value.
 |#

(define-llvm LLVMTypeOf (_fun LLVMValueRef -> LLVMTypeRef))
(define-llvm LLVMGetValueKind (_fun LLVMValueRef -> LLVMValueKind))
(define-llvm LLVMGetValueName (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetValueName (_fun LLVMValueRef _string -> _void))
(define-llvm LLVMDumpValue (_fun LLVMValueRef -> _void))
(define-llvm LLVMPrintValueToString (_fun LLVMValueRef -> _string))
(define-llvm LLVMReplaceAllUsesWith (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMIsConstant (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMIsUndef (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMIsAMDNode (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMIsAMDString (_fun LLVMValueRef -> LLVMValueRef))

;; LLVMCCoreValueUses
#|
 * This module defines functions that allow you to inspect the uses of a
 * LLVMValueRef.
 *
 * It is possible to obtain an LLVMUseRef for any LLVMValueRef instance.
 * Each LLVMUseRef (which corresponds to a llvm::Use instance) holds a
 * llvm::User and llvm::Value.
 |#

(define-llvm LLVMGetFirstUse (_fun LLVMValueRef -> LLVMUseRef))
(define-llvm LLVMGetNextUse (_fun LLVMUseRef -> LLVMUseRef))
(define-llvm LLVMGetUser (_fun LLVMUseRef -> LLVMValueRef))
(define-llvm LLVMGetUsedValue (_fun LLVMUseRef -> LLVMValueRef))

;; LLVMCCoreValueUser
#|
 * Function in this group pertain to LLVMValueRef instances that descent
 * from llvm::User. This includes constants, instructions, and
 * operators.
 |#

(define-llvm LLVMGetOperand (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMGetOperandUse (_fun LLVMValueRef _uint -> LLVMUseRef))
(define-llvm LLVMSetOperand (_fun LLVMValueRef _uint LLVMValueRef -> _void))
(define-llvm LLVMGetNumOperands (_fun LLVMValueRef -> _int))


;; LLVMCCoreValueConstant
#|
 * This section contains APIs for interacting with LLVMValueRef that
 * correspond to llvm::Constant instances.
 *
 * These functions will work for any LLVMValueRef in the llvm::Constant
 * class hierarchy.
 |#

(define-llvm LLVMConstNull (_fun LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstAllOnes (_fun LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMGetUndef (_fun LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMIsNull (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMConstPointerNull (_fun LLVMTypeRef -> LLVMValueRef))

;; LLVMCCoreValueConstantScalar
#|
 * Functions in this group model LLVMValueRef instances that correspond
 * to constants referring to scalar types.
 *
 * For integer types, the LLVMTypeRef parameter should correspond to a
 * llvm::IntegerType instance and the returned LLVMValueRef will
 * correspond to a llvm::ConstantInt.
 *
 * For floating point types, the LLVMTypeRef returned corresponds to a
 * llvm::ConstantFP.
 |#

(define-llvm LLVMConstInt (_fun LLVMTypeRef _ullong LLVMBool -> LLVMValueRef))
(define-llvm LLVMConstIntOfArbitraryPrecision (_fun LLVMTypeRef _uint _uint64 -> LLVMValueRef))
(define-llvm LLVMConstIntOfString (_fun LLVMTypeRef _string _uint8 -> LLVMValueRef))
(define-llvm LLVMConstIntOfStringAndSize (_fun LLVMTypeRef _string _uint _uint8 -> LLVMValueRef))
(define-llvm LLVMConstReal (_fun LLVMTypeRef _double -> LLVMValueRef))
(define-llvm LLVMConstRealOfString (_fun LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMConstRealOfStringAndSize (_fun LLVMTypeRef _string _uint -> LLVMValueRef))
(define-llvm LLVMConstIntGetZExtValue (_fun LLVMValueRef -> _ullong))
(define-llvm LLVMConstIntGetSExtValue (_fun LLVMValueRef -> _llong))
(define-llvm LLVMConstRealGetDouble (_fun LLVMValueRef (pointer-to LLVMBool) -> _double))

;; LLVMCCoreValueConstantComposite
#|
 * Functions in this group operate on composite constants.
 |#

(define-llvm LLVMConstStringInContext (_fun LLVMContextRef _string _uint LLVMBool -> LLVMValueRef))
(define-llvm LLVMConstString (_fun _string _uint LLVMBool -> LLVMValueRef))
(define-llvm LLVMIsConstantString (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMGetAsString (_fun LLVMValueRef (pointer-to _size) -> _string))
(define-llvm LLVMConstStructInContext (_fun LLVMContextRef
                                            (vals : (_list i LLVMValueRef)) (len : _uint = (length vals)) LLVMBool
                                            -> LLVMValueRef))
(define-llvm LLVMConstStruct (_fun (vals : (_list i LLVMValueRef)) (len : _uint = (length vals)) LLVMBool -> LLVMValueRef))
(define-llvm LLVMConstArray (_fun LLVMTypeRef (vals : (_list i  LLVMValueRef)) (len : _uint = (length vals)) -> LLVMValueRef))
(define-llvm LLVMConstNamedStruct (_fun LLVMTypeRef (vals : (_list i LLVMValueRef)) (len : _uint = (length vals))
                                        -> LLVMValueRef))
(define-llvm LLVMGetElementAsConstant (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMConstVector (_fun (vals : (_list i  LLVMValueRef)) (len : _uint = (length vals)) -> LLVMValueRef))

;; LLVMCCoreValueConstantExpressions
#|
 * Functions in this group correspond to APIs on llvm::ConstantExpr.
 |#
(define-llvm LLVMGetConstOpcode (_fun LLVMValueRef -> LLVMOpcode))
(define-llvm LLVMAlignOf (_fun LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMSizeOf (_fun LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstNeg (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNSWNeg (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNUWNeg (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFNeg (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNot (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstAdd (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNSWAdd (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNUWAdd (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFAdd (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstSub (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNSWSub (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNUWSub (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFSub (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstMul (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNSWMul (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstNUWMul (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFMul (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstUDiv (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstSDiv (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstExactSDiv (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFDiv (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstURem (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstSRem (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFRem (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstAnd (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstOr (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstXor (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstICmp (_fun LLVMIntPredicate LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFCmp (_fun LLVMRealPredicate LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstShl (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstLShr (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstAShr (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstGEP (_fun LLVMValueRef (pointer-to LLVMValueRef) _uint -> LLVMValueRef))
(define-llvm LLVMConstInBoundsGEP (_fun LLVMValueRef (pointer-to LLVMValueRef) _uint -> LLVMValueRef))
(define-llvm LLVMConstTrunc (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstSExt (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstZExt (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstFPTrunc (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstFPExt (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstUIToFP (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstSIToFP (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstFPToUI (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstFPToSI (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstPtrToInt (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstIntToPtr (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstBitCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstAddrSpaceCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstZExtOrBitCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstSExtOrBitCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstTruncOrBitCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstPointerCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstIntCast (_fun LLVMValueRef LLVMTypeRef LLVMBool -> LLVMValueRef))
(define-llvm LLVMConstFPCast (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))
(define-llvm LLVMConstSelect (_fun LLVMValueRef LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstExtractElement (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstInsertElement (_fun LLVMValueRef LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstShuffleVector (_fun LLVMValueRef LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstExtractValue (_fun LLVMValueRef (pointer-to unsigned) _uint -> LLVMValueRef))
(define-llvm LLVMConstInsertValue (_fun LLVMValueRef LLVMValueRef (pointer-to unsigned) _uint -> LLVMValueRef))
(define-llvm LLVMConstInlineAsm (_fun LLVMTypeRef _string _string LLVMBool LLVMBool -> LLVMValueRef))
(define-llvm LLVMBlockAddress (_fun LLVMValueRef LLVMBasicBlockRef -> LLVMValueRef))


;; LLVMCCoreValueConstantGlobals
#|
 * This group contains functions that operate on global values. Functions in
 * this group relate to functions in the llvm::GlobalValue class tree.
 |#

(define-llvm LLVMGetGlobalParent (_fun LLVMValueRef -> LLVMModuleRef))
(define-llvm LLVMIsDeclaration (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMGetLinkage (_fun LLVMValueRef -> LLVMLinkage))
(define-llvm LLVMSetLinkage (_fun LLVMValueRef LLVMLinkage -> _void))
(define-llvm LLVMGetSection (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetSection (_fun LLVMValueRef _string -> _void))
(define-llvm LLVMGetVisibility (_fun LLVMValueRef -> LLVMVisibility))
(define-llvm LLVMSetVisibility (_fun LLVMValueRef LLVMVisibility -> _void))
(define-llvm LLVMGetDLLStorageClass (_fun LLVMValueRef -> LLVMDLLStorageClass))
(define-llvm LLVMSetDLLStorageClass (_fun LLVMValueRef LLVMDLLStorageClass -> _void))
(define-llvm LLVMHasUnnamedAddr (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetUnnamedAddr (_fun LLVMValueRef LLVMBool -> _void))
(define-llvm LLVMGetAlignment (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMSetAlignment (_fun LLVMValueRef _uint -> _void))

;; LLVMCoreValueConstantGlobalVariable
#|
 * This group contains functions that operate on global variable values.
 |#

(define-llvm LLVMAddGlobal (_fun LLVMModuleRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMAddGlobalInAddressSpace (_fun LLVMModuleRef LLVMTypeRef _string _uint -> LLVMValueRef))
(define-llvm LLVMGetNamedGlobal (_fun LLVMModuleRef _string -> LLVMValueRef))
(define-llvm LLVMGetFirstGlobal (_fun LLVMModuleRef -> LLVMValueRef))
(define-llvm LLVMGetLastGlobal (_fun LLVMModuleRef -> LLVMValueRef))
(define-llvm LLVMGetNextGlobal (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetPreviousGlobal (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMDeleteGlobal (_fun LLVMValueRef -> _void))
(define-llvm LLVMGetInitializer (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMSetInitializer (_fun LLVMValueRef LLVMValueRef/null -> _void))
(define-llvm LLVMIsThreadLocal (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetThreadLocal (_fun LLVMValueRef LLVMBool -> _void))
(define-llvm LLVMIsGlobalConstant (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetGlobalConstant (_fun LLVMValueRef LLVMBool -> _void))
(define-llvm LLVMGetThreadLocalMode (_fun LLVMValueRef -> LLVMThreadLocalMode))
(define-llvm LLVMSetThreadLocalMode (_fun LLVMValueRef LLVMThreadLocalMode -> _void))
(define-llvm LLVMIsExternallyInitialized (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetExternallyInitialized (_fun LLVMValueRef LLVMBool -> _void))

;; LLVMCoreValueConstantGlobalAlias
(define-llvm LLVMAddAlias (_fun LLVMModuleRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))

;; LLVMCCoreValueFunction
(define-llvm LLVMDeleteFunction (_fun LLVMValueRef -> _void))
(define-llvm LLVMGetPersonalityFn (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMSetPersonalityFn (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMGetIntrinsicID (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetFunctionCallConv (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMSetFunctionCallConv (_fun LLVMValueRef _uint -> _void))
(define-llvm LLVMGetGC (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetGC (_fun LLVMValueRef _string -> _void))

(define-llvm LLVMAddAttributeAtIndex (_fun LLVMValueRef LLVMAttributeIndex LLVMAttributeRef -> _void))
(define-llvm LLVMGetAttributeCountAtIndex (_fun LLVMValueRef LLVMAttributeIndex -> unsigned))
(define-llvm LLVMGetAttributesAtIndex (_fun LLVMValueRef LLVMAttributeIndex (pointer-to LLVMAttributeRef ) -> _void))
(define-llvm LLVMGetEnumAttributeAtIndex (_fun LLVMValueRef LLVMAttributeIndex _uint -> LLVMAttributeRef))
(define-llvm LLVMGetStringAttributeAtIndex (_fun LLVMValueRef LLVMAttributeIndex _string _uint -> LLVMAttributeRef))
(define-llvm LLVMRemoveEnumAttributeAtIndex (_fun LLVMValueRef LLVMAttributeIndex _uint -> _void))
(define-llvm LLVMRemoveStringAttributeAtIndex (_fun LLVMValueRef LLVMAttributeIndex _string _uint -> _void))
(define-llvm LLVMAddTargetDependentFunctionAttr (_fun LLVMValueRef _string _string -> _void))


;; LLVMCCoreValueFunctionParameters
(define-llvm LLVMCountParams (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetParams (_fun LLVMValueRef (pointer-to LLVMValueRef) -> _void))
(define-llvm LLVMGetParam (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMGetParamParent (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetFirstParam (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetLastParam (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetNextParam (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMGetPreviousParam (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMSetParamAlignment (_fun LLVMValueRef _uint -> _void))

;; LLVMCCoreValueMetadata

(define-llvm LLVMMDStringInContext (_fun LLVMContextRef _string _uint -> LLVMValueRef))
(define-llvm LLVMMDString (_fun _string _uint -> LLVMValueRef))
(define-llvm LLVMMDNodeInContext (_fun LLVMContextRef (pointer-to LLVMValueRef) _uint -> LLVMValueRef))
(define-llvm LLVMMDNode (_fun (vals : (_list i  LLVMValueRef)) (size : _uint = (length vals)) -> LLVMValueRef))
;(define-llvm LLVMMetadataAsValue (_fun LLVMContextRef LLVMMetadataRef -> LLVMValueRef))
;(define-llvm LLVMValueAsMetadata (_fun LLVMValueRef -> LLVMMetadataRef))
(define-llvm LLVMGetMDString (_fun LLVMValueRef (pointer-to unsigned) -> _string))
(define-llvm LLVMGetMDNodeNumOperands (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetMDNodeOperands (_fun LLVMValueRef (pointer-to LLVMValueRef) -> _void))

;; LLVMCCoreValueBasicBlock
#|
* A basic block represents a single entry single exit section of code.
* Basic blocks contain a list of instructions which form the body of
* the block.
*
* Basic blocks belong to functions. They have the type of label.
*
* Basic blocks are themselves values. However, the C API models them as
* LLVMBasicBlockRef.
|#

(define-llvm LLVMBasicBlockAsValue (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMValueIsBasicBlock (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMValueAsBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetBasicBlockParent (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMGetBasicBlockTerminator (_fun LLVMBasicBlockRef -> LLVMValueRef/null))
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

;; LLVMCCoreValueInstruction
#|
 * Functions in this group relate to the inspection and manipulation of
 * individual instructions.
 *
 * In the C++ API, an instruction is modeled by llvm::Instruction. This
 * class has a large number of descendents. llvm::Instruction is a
 * llvm::Value and in the C API, instructions are modeled by
 * LLVMValueRef.
 *
 * This group also contains sub-groups which operate on specific
 * llvm::Instruction types, e.g. llvm::CallInst.
 |#

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

;; LLVMCCoreValueInstructionCall
(define-llvm LLVMGetNumArgOperands (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMSetInstructionCallConv (_fun LLVMValueRef _uint -> _void))
(define-llvm LLVMGetInstructionCallConv (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMSetInstrParamAlignment (_fun LLVMValueRef _uint _uint -> _void))
(define-llvm LLVMAddCallSiteAttribute (_fun LLVMValueRef LLVMAttributeIndex LLVMAttributeRef -> _void))
(define-llvm LLVMGetCallSiteAttributeCount (_fun LLVMValueRef LLVMAttributeIndex -> unsigned))
(define-llvm LLVMGetCallSiteAttributes (_fun LLVMValueRef LLVMAttributeIndex (pointer-to LLVMAttributeRef) -> _void))
(define-llvm LLVMGetCallSiteEnumAttribute (_fun LLVMValueRef LLVMAttributeIndex _uint -> LLVMAttributeRef))
(define-llvm LLVMGetCallSiteStringAttribute (_fun LLVMValueRef LLVMAttributeIndex _string _uint -> LLVMAttributeRef))
(define-llvm LLVMRemoveCallSiteEnumAttribute (_fun LLVMValueRef LLVMAttributeIndex _uint -> _void))
(define-llvm LLVMRemoveCallSiteStringAttribute (_fun LLVMValueRef LLVMAttributeIndex _string _uint -> _void))
(define-llvm LLVMGetCalledValue (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMIsTailCall (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetTailCall (_fun LLVMValueRef LLVMBool -> _void))
(define-llvm LLVMGetNormalDest (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetUnwindDest (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMSetNormalDest (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMSetUnwindDest (_fun LLVMValueRef -> LLVMBasicBlockRef))

;; LLVMCCoreValueInstructionTerminator
(define-llvm LLVMGetNumSuccessors (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetSuccessor (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))
(define-llvm LLVMSetSuccessor (_fun LLVMValueRef _uint LLVMBasicBlockRef -> _void))
(define-llvm LLVMIsConditional (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMGetCondition (_fun LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMSetCondition (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMGetSwitchDefaultDest (_fun LLVMValueRef -> LLVMBasicBlockRef))

;; LLVMCCoreValueInstructionAlloca
(define-llvm LLVMGetAllocatedType (_fun LLVMValueRef -> LLVMTypeRef))

;; LLVMCCoreValueInstructionGetElementPointer
(define-llvm LLVMIsInBounds (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetIsInBounds (_fun LLVMValueRef LLVMBool -> _void))

;; LLVMCCoreValueInstructionPHINode
(define-llvm LLVMAddIncoming
  (_fun LLVMValueRef
        (values : (_list i LLVMValueRef))
        (_list i LLVMBasicBlockRef)
        (size : _uint = (length values)) -> _void))
(define-llvm LLVMCountIncoming (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetIncomingValue (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMGetIncomingBlock (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))

;; LLVMCCoreValueInstructionExtractValue
;; LLVMCCoreValueInstructionInsertValue
(define-llvm LLVMGetNumIndices (_fun LLVMValueRef -> unsigned))
(define-llvm LLVMGetIndices (_fun LLVMValueRef -> unsigned))

;; LLVMCCoreInstructionBuilder
#|
 * An instruction builder represents a point within a basic block and is
 * the exclusive means of building instructions using the C interface.
 |#

(define-llvm LLVMCreateBuilderInContext (_fun LLVMContextRef -> LLVMBuilderRef))
(define-llvm LLVMCreateBuilder (_fun -> LLVMBuilderRef))
(define-llvm LLVMPositionBuilder (_fun LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef -> _void))
(define-llvm LLVMPositionBuilderBefore (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMPositionBuilderAtEnd (_fun LLVMBuilderRef LLVMBasicBlockRef -> _void))
(define-llvm LLVMGetInsertBlock (_fun LLVMBuilderRef -> LLVMBasicBlockRef))
(define-llvm LLVMClearInsertionPosition (_fun LLVMBuilderRef -> _void))
(define-llvm LLVMInsertIntoBuilder (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMInsertIntoBuilderWithName (_fun LLVMBuilderRef LLVMValueRef _string -> _void))
(define-llvm LLVMDisposeBuilder (_fun LLVMBuilderRef -> _void))

;; Metadata
(define-llvm LLVMSetCurrentDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMGetCurrentDebugLocation (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMSetInstDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))

;; Terminators
(define-llvm LLVMBuildRetVoid (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMBuildRet (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMBuildAggregateRet (_fun LLVMBuilderRef [retvals : (_list i LLVMValueRef)] [_uint = (length retvals)] -> LLVMValueRef))
(define-llvm LLVMBuildBr (_fun LLVMBuilderRef LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMBuildCondBr (_fun LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMBuildSwitch (_fun LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef _uint -> LLVMValueRef))
(define-llvm LLVMBuildIndirectBr (_fun LLVMBuilderRef LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMBuildInvoke (_fun LLVMBuilderRef LLVMValueRef [args : (_list i LLVMValueRef)] [_uint = (length args)] LLVMBasicBlockRef LLVMBasicBlockRef _string -> LLVMValueRef))
(define-llvm LLVMBuildLandingPad (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _uint _string -> LLVMValueRef))
(define-llvm LLVMBuildResume (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMBuildUnreachable (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMAddCase (_fun LLVMValueRef LLVMValueRef LLVMBasicBlockRef -> _void))
(define-llvm LLVMAddDestination (_fun LLVMValueRef LLVMBasicBlockRef -> _void))
(define-llvm LLVMAddClause (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMSetCleanup (_fun LLVMValueRef LLVMBool -> _void))

;; Arithmetic
(define-llvm LLVMBuildAdd (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNSWAdd (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNUWAdd (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFAdd (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildSub (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNSWSub (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNUWSub (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFSub (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildMul (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNSWMul (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNUWMul (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFMul (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildUDiv (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildSDiv (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildExactSDiv (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFDiv (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildURem (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildSRem (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFRem (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildShl (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildLShr (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildAShr (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildAnd (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildOr (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildXor (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildBinOp (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNeg (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNSWNeg (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNUWNeg (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFNeg (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildNot (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))

;; Memory
(define-llvm LLVMBuildMalloc (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildArrayMalloc (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildAlloca (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildArrayAlloca (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFree (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMBuildLoad (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildStore (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMBuildGEP (_fun LLVMBuilderRef LLVMValueRef (indices : (_list i LLVMValueRef)) (len : _uint = (length indices)) _string -> LLVMValueRef))
(define-llvm LLVMBuildInBoundsGEP (_fun LLVMBuilderRef LLVMValueRef (indices : (_list i LLVMValueRef)) (len : _uint = (length indices)) _string -> LLVMValueRef))
(define-llvm LLVMBuildStructGEP (_fun LLVMBuilderRef LLVMValueRef _uint _string -> LLVMValueRef))
(define-llvm LLVMBuildGlobalString (_fun LLVMBuilderRef _string _string -> LLVMValueRef))
(define-llvm LLVMBuildGlobalStringPtr (_fun LLVMBuilderRef _string _string -> LLVMValueRef))
(define-llvm LLVMGetVolatile (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetVolatile (_fun LLVMValueRef LLVMBool -> _void))
(define-llvm LLVMGetOrdering (_fun LLVMValueRef -> LLVMAtomicOrdering))
(define-llvm LLVMSetOrdering (_fun LLVMValueRef LLVMAtomicOrdering -> _void))

;; Casts
(define-llvm LLVMBuildTrunc (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildZExt (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildSExt (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFPToUI (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFPToSI (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildUIToFP (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildSIToFP (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFPTrunc (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFPExt (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildPtrToInt (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildIntToPtr (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildBitCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildAddrSpaceCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildZExtOrBitCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildSExtOrBitCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildTruncOrBitCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildCast (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildPointerCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildIntCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFPCast (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

;; Comparisons
(define-llvm LLVMBuildICmp (_fun LLVMBuilderRef LLVMIntPredicate LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFCmp (_fun LLVMBuilderRef LLVMRealPredicate LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

;; Miscellaneous instructions
(define-llvm LLVMBuildPhi (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildCall (_fun LLVMBuilderRef LLVMValueRef [args : (_list i LLVMValueRef)] [_uint = (length args)]  _string -> LLVMValueRef))
(define-llvm LLVMBuildSelect (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildVAArg (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMBuildExtractElement (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildInsertElement (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildShuffleVector (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildExtractValue (_fun LLVMBuilderRef LLVMValueRef _uint _string -> LLVMValueRef))
(define-llvm LLVMBuildInsertValue (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _uint _string -> LLVMValueRef))
(define-llvm LLVMBuildIsNull (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildIsNotNull (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildPtrDiff (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildFence (_fun LLVMBuilderRef LLVMAtomicOrdering LLVMBool _string -> LLVMValueRef))
(define-llvm LLVMBuildAtomicRMW (_fun LLVMBuilderRef LLVMAtomicRMWBinOp LLVMValueRef LLVMValueRef LLVMAtomicOrdering LLVMBool -> LLVMValueRef))
(define-llvm LLVMIsAtomicSingleThread (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetAtomicSingleThread (_fun LLVMValueRef LLVMBool -> _void))
(define-llvm LLVMGetCmpXchgSuccessOrdering (_fun LLVMValueRef -> LLVMAtomicOrdering))
(define-llvm LLVMSetCmpXchgSuccessOrdering (_fun LLVMValueRef LLVMAtomicOrdering -> _void))
(define-llvm LLVMGetCmpXchgFailureOrdering (_fun LLVMValueRef -> LLVMAtomicOrdering))
(define-llvm LLVMSetCmpXchgFailureOrdering (_fun LLVMValueRef LLVMAtomicOrdering -> _void))


;; LLVMCCoreModuleProvider
; Deprecated
(define-llvm LLVMDisposeModuleProvider (_fun LLVMModuleProviderRef -> _void)
  #:wrap (deallocator))
(define-llvm LLVMCreateModuleProviderForExistingModule (_fun LLVMModuleRef -> LLVMModuleProviderRef)
  #:wrap (allocator LLVMDisposeModuleProvider))

;; LLVMCCoreMemoryBuffers Memory Buffers
(define-llvm LLVMDisposeMemoryBuffer (_fun LLVMMemoryBufferRef -> _void)
  #:wrap (deallocator))
(define-llvm LLVMCreateMemoryBufferWithContentsOfFile
  (_fun _string
        (memory-buffer : (_ptr o LLVMMemoryBufferRef))
        (out-message : (_ptr o _string))
        -> (fail : LLVMBool)
        -> (vector memory-buffer out-message fail))
  #:wrap (allocator LLVMDisposeMemoryBuffer))
(define-llvm LLVMCreateMemoryBufferWithSTDIN
  (_fun (memory-buffer : (_ptr o LLVMMemoryBufferRef))
        (out-message : (_ptr o _string))
        -> (fail : LLVMBool)
        -> (vector memory-buffer out-message fail))
  #:wrap (allocator LLVMDisposeMemoryBuffer))
(define-llvm LLVMCreateMemoryBufferWithMemoryRange
  (_fun _string _size _string LLVMBool -> LLVMMemoryBufferRef)
  #:wrap (allocator LLVMDisposeMemoryBuffer))
(define-llvm LLVMCreateMemoryBufferWithMemoryRangeCopy
  (_fun _string _size _string -> LLVMMemoryBufferRef)
  #:wrap (allocator LLVMDisposeMemoryBuffer))
(define-llvm LLVMGetBufferStart (_fun LLVMMemoryBufferRef -> _bytes))
(define-llvm LLVMGetBufferSize (_fun LLVMMemoryBufferRef -> _size))


;; LLVMCCorePassManagers
(define-llvm LLVMDisposePassManager (_fun LLVMPassManagerRef -> _void)
  #:wrap (deallocator))
(define-llvm LLVMCreatePassManager (_fun -> LLVMPassManagerRef)
  #:wrap (allocator LLVMDisposePassManager))
(define-llvm LLVMCreateFunctionPassManagerForModule (_fun LLVMModuleRef -> LLVMPassManagerRef)
  #:wrap (allocator LLVMDisposePassManager))
(define-llvm LLVMRunPassManager (_fun LLVMPassManagerRef LLVMModuleRef -> LLVMBool))
(define-llvm LLVMInitializeFunctionPassManager (_fun LLVMPassManagerRef -> LLVMBool))
(define-llvm LLVMRunFunctionPassManager (_fun LLVMPassManagerRef LLVMValueRef -> LLVMBool))
(define-llvm LLVMFinalizeFunctionPassManager (_fun LLVMPassManagerRef -> LLVMBool))
