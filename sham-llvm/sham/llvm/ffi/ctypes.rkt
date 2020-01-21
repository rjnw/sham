#lang racket/base

(require (for-syntax racket/base syntax/parse racket/syntax))
(require ffi/unsafe)

(require "define.rkt")

(provide (all-defined-out))
(define LLVMBool _bool)

(define-syntax (define-llvm-types stx)
  (define-syntax-class id-pair
   (pattern (type-name:id cons:id acc:id)))
  (syntax-parse stx
    ((_ p ...)
     #:with (p? ...) (map (λ (v) (format-id v "~a?" v)) (syntax-e #'(p ...)))
     #`(begin
         (begin
           (define p (_cpointer 'p))
           (define (p? v) (cpointer-has-tag? v 'p)))
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
  LLVMPassManagerBuilderRef
  LLVMUseRef           ;;Used to get the users and usees of a Value.
  LLVMDiagnosticInfoRef;;llvm::DiagnosticInfo
  LLVMAttributeRef     ;;Used to represent an attributes.
  LLVMMetadataRef      ;;represents metadata value

  LLVMGenericValueRef
  LLVMExecutionEngineRef
  LLVMTargetDataRef
  LLVMTargetLibraryInfoRef
  LLVMMCJITMemoryManagerRef)

(define LLVMValueRef/null (_cpointer/null 'LLVMValueRef))

(define LLVMAttribute
  (_bitmask
   `(LLVMZExtAttribute            = ,(arithmetic-shift 1  0)
     LLVMSExtAttribute            = ,(arithmetic-shift 1  1)
     LLVMNoReturnAttribute        = ,(arithmetic-shift 1  2)
     LLVMInRegAttribute           = ,(arithmetic-shift 1  3)
     LLVMStructRetAttribute       = ,(arithmetic-shift 1  4)
     LLVMNoUnwindAttribute        = ,(arithmetic-shift 1  5)
     LLVMNoAliasAttribute         = ,(arithmetic-shift 1  6)
     LLVMByValAttribute           = ,(arithmetic-shift 1  7)
     LLVMNestAttribute            = ,(arithmetic-shift 1  8)
     LLVMReadNoneAttribute        = ,(arithmetic-shift 1  9)
     LLVMReadOnlyAttribute        = ,(arithmetic-shift 1  10)
     LLVMNoInlineAttribute        = ,(arithmetic-shift 1  11)
     LLVMAlwaysInlineAttribute    = ,(arithmetic-shift 1  12)
     LLVMOptimizeForSizeAttribute = ,(arithmetic-shift 1  13)
     LLVMStackProtectAttribute    = ,(arithmetic-shift 1  14)
     LLVMStackProtectReqAttribute = ,(arithmetic-shift 1  15)
     LLVMAlignment                = ,(arithmetic-shift 31 16)
     LLVMNoCaptureAttribute       = ,(arithmetic-shift 1  21)
     LLVMNoRedZoneAttribute       = ,(arithmetic-shift 1  22)
     LLVMNoImplicitFloatAttribute = ,(arithmetic-shift 1  23)
     LLVMNakedAttribute           = ,(arithmetic-shift 1  24)
     LLVMInlineHintAttribute      = ,(arithmetic-shift 1  25)
     LLVMStackAlignment           = ,(arithmetic-shift 7  26)
     LLVMReturnsTwice             = ,(arithmetic-shift 1  29)
     LLVMUWTable                  = ,(arithmetic-shift 1  30)
     LLVMNonLazyBind              = ,(arithmetic-shift 1  31))))

(define (pointer-to p) _pointer)
(define unsigned _uint)
