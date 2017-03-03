#lang racket/base

(require (for-syntax racket/base syntax/parse))
(require ffi/unsafe)

(require "define.rkt")

(provide
 define-llvm-types
 LLVMBool
 LLVMMemoryBufferRef
 LLVMContextRef
 LLVMModuleRef
 LLVMTypeRef
 LLVMValueRef
 LLVMBasicBlockRef
 LLVMBuilderRef
 LLVMModuleProviderRef
 LLVMPassManagerRef
 LLVMPassRegistryRef
 LLVMUseRef
 LLVMDiagnosticInfoRef

 LLVMOpcode
 LLVMTypeKind
 LLVMLinkage
 LLVMVisibility
 LLVMCallConv
 LLVMIntPredicate
 LLVMRealPredicate
 LLVMAtomicOrdering
 LLVMAtomicRMWBinOp)

(define LLVMBool _bool)

(define-syntax (define-llvm-types stx)
  (define-syntax-class id-pair
   (pattern (type-name:id cons:id acc:id)))
  (syntax-parse stx
   ((_ p ...)
    #'(begin
        (define p (_cpointer 'p))
        ...))))

(define-llvm-types
  LLVMMemoryBufferRef  ;;Used to pass regions of memory through LLVM interfaces
  LLVMContextRef       ;;The top-level container for all LLVM global data. See the LLVMContext class.
  LLVMModuleRef        ;;The top-level container for all other LLVM Intermediate Representation (IR)
  LLVMTypeRef          ;;Each value in the LLVM IR has a type, an LLVMTypeRef.
  LLVMValueRef         ;;Represents an individual value in LLVM IR.
  LLVMBasicBlockRef    ;;Represents a basic block of instructions in LLVM IR.
  LLVMBuilderRef       ;;Represents an LLVM basic block builder.
  LLVMModuleProviderRef;;Interface used to provide a module to JIT or interpreter.
  LLVMPassManagerRef   ;;llvm::PassManagerBase
  LLVMPassRegistryRef  ;;llvm::PassRegistry
  LLVMUseRef           ;;Used to get the users and usees of a Value.
  LLVMDiagnosticInfoRef;;llvm::DiagnosticInfo

  ;; LLVMTargetDataRef
  ;; LLVMExecutionEngineRef
  ;; LLVMGenericValueRef
  ;; LLVMContextRef
  ;; LLVMModuleProviderRef
  ;; LLVMPassManagerRef
  ;; LLVMPassRegistryRef
  ;; LLVMUseRef
  ;; LLVMTargetDataRef
  )

;;enumeration
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
(define LLVMAtomicOrdering
  (_enum
   '(LLVMAtomicOrderingNotAtomic              = 0
     LLVMAtomicOrderingUnordered              = 1
     LLVMAtomicOrderingMonotonic              = 2
     LLVMAtomicOrderingAcquire                = 4
     LLVMAtomicOrderingRelease                = 5
     LLVMAtomicOrderingAcquireRelease         = 6
     LLVMAtomicOrderingSequentiallyConsistent = 7 )))

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
  (_enum '(LLVMLandingPadCatch LLVMLandingPadFilter )))
(define LLVMThreadLocalMode
  (_enum
   '(LLVMNotThreadLocal = 0
     LLVMGeneralDynamicTLSModel
     LLVMLocalDynamicTLSModel
     LLVMInitialExecTLSModel
     LLVMLocalExecTLSModel)))
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
(define LLVMVisibility
  (_enum
   '(LLVMDefaultVisibility
     LLVMHiddenVisibility
     LLVMProtectedVisibility)))
(define LLVMCallConv
  (_enum
   '(LLVMCCallConv = 0
     LLVMFastCallConv = 8
     LLVMColdCallConv = 9
     LLVMWebKitJSCallConv = 12
     LLVMAnyRegCallConv = 13
     LLVMX86StdcallCallConv = 64
     LLVMX86FastcallCallConv = 65)))
