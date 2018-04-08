#lang racket
(require "define.rkt"
         "ctypes.rkt"
         "target-machine.rkt"
         ffi/unsafe)

(provide (all-defined-out))

(define LLVMOrcJITStackRef _pointer)
(define LLVMOrcModuleHandle _uint32)
(define LLVMOrcTargetAddress _uint64)
(define LLVMSharedModuleRef _pointer)
(define LLVMSharedObjectBufferRef _pointer)
(define LLVMOrcSymbolResolverFn (_fun _string _pointer -> _uint64))
(define LLVMOrcLazyCompileCallbackFn (_fun LLVMOrcJITStackRef _pointer -> _uint64))

(define LLVMOrcErrorCode (_enum '(LLVMOrcErrSuccess = 0 LLVMOrcErrGeneric)))

(define-llvm LLVMOrcMakeSharedModule (_fun LLVMModuleRef -> LLVMSharedModuleRef))
(define-llvm LLVMOrcDisposeSharedModuleRef (_fun LLVMSharedModuleRef -> _void))
;; (define-llvm LLVMOrcMakeSharedObjectBuffer
;;   (_fun LLVMMemoryBufferRef -> LLVMSharedObjectBufferRef))

(define-llvm LLVMOrcCreateInstance
  (_fun LLVMTargetMachineRef -> LLVMOrcJITStackRef))
(define-llvm LLVMOrcGetErrorMsg (_fun LLVMOrcJITStackRef -> _string))
(define-llvm LLVMOrcGetMangledSymbol
  (_fun LLVMOrcJITStackRef (mangled-symbol : (_ptr o _string)) _string
        -> _void -> mangled-symbol))
;; (define-llvm LLVMDisposeMangledSymbol (_fun _string -> _void))

(define-llvm LLVMOrcCreateLazyCompileCallback
  (_fun LLVMOrcJITStackRef LLVMOrcLazyCompileCallbackFn _pointer
        -> LLVMOrcTargetAddress))
(define-llvm LLVMOrcCreateIndirectStub
  (_fun LLVMOrcJITStackRef _string LLVMOrcTargetAddress -> LLVMOrcErrorCode))
(define-llvm LLVMOrcSetIndirectStubPointer
  (_fun LLVMOrcJITStackRef _string LLVMOrcTargetAddress -> LLVMOrcErrorCode))
(define-llvm LLVMOrcAddEagerlyCompiledIR
  (_fun LLVMOrcJITStackRef (module-handle : (_ptr o LLVMOrcModuleHandle))
        LLVMSharedModuleRef _pointer _pointer
        -> (error-code : LLVMOrcErrorCode)
        -> (values error-code module-handle)))
(define-llvm LLVMOrcAddLazilyCompiledIR
  (_fun LLVMOrcJITStackRef LLVMModuleRef LLVMOrcSymbolResolverFn _pointer
        -> LLVMOrcModuleHandle))
;; TODO add object.h
; (define-llvm LLVMOrcAddObjectFile (_fun LLVMOrcJITStackRef LLVMObjectFileRef LLVMOrcSymbolResolverFn _pointer -> LLVMOrcModuleHandle))
(define-llvm LLVMOrcGetSymbolAddress
  (_fun LLVMOrcJITStackRef (target-address : (_ptr o LLVMOrcTargetAddress)) _string
        -> (error-code : LLVMOrcTargetAddress)
        -> (values error-code target-address)))
(define-llvm LLVMOrcDisposeInstance (_fun LLVMOrcJITStackRef -> _void))
