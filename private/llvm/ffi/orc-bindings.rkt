#lang racket
(require "define.rkt"
         "ctypes.rkt"
         ffi/unsafe)

(provide (all-defined-out))

(define LLVMOrcJITStackRef _pointer)
(define LLVMOrcModuleHandle _uint32)
(define LLVMOrcTargetAddress _uint64)
(define LLVMOrcSymbolResolverFn (_fun _string _pointer -> _uint64))
(define LLVMOrcLazyCompileCallbackFn (_fun LLVMOrcJITStackRef _pointer -> _uint64))

(define LLVMOrcErrorCode (_enum '(LLVMOrcErrSuccess = 0 LLVMOrcErrGeneric)))

(define LLVMOrcCreateInstance (_fun LLVMTargetMachineRef -> LLVMOrcJITStackRef))
(define LLVMOrcGetErrorMsg (_fun LLVMOrcJITStackRef -> _string))
(define LLVMOrcGetMangledSymbol (_fun LLVMOrcJITStackRef (mangled-symbol : (_ptr o _string)) _string -> _void -> mangled-symbol))
(define LLVMDisposeMangledSymbol (_fun _string -> _void))

(define-llvm LLVMOrcCreateLazyCompileCallback (_fun LLVMOrcJITStackRef LLVMOrcLazyCompileCallbackFn _pointer -> LLVMOrcTargetAddress))
(define-llvm LLVMOrcCreateIndirectStub (_fun LLVMOrcJITStackRef _string LLVMOrcTargetAddress -> LLVMOrcErrorCode))
(define-llvm LLVMOrcSetIndirectStubPointer (_fun LLVMOrcJITStackRef _string LLVMOrcTargetAddress -> LLVMOrcErrorCode))
(define-llvm LLVMOrcAddEagerlyCompiledIR (_fun LLVMOrcJITStackRef LLVMModuleRef LLVMOrcSymbolResolverFn _pointer -> LLVMOrcModuleHandle))
(define-llvm LLVMOrcAddLazilyCompiledIR (_fun LLVMOrcJITStackRef LLVMModuleRef LLVMOrcSymbolResolverFn _pointer -> LLVMOrcModuleHandle))
;; TODO add object.h
; (define-llvm LLVMOrcAddObjectFile (_fun LLVMOrcJITStackRef LLVMObjectFileRef LLVMOrcSymbolResolverFn _pointer -> LLVMOrcModuleHandle))
(define-llvm LLVMOrcGetSymbolAddress (_fun LLVMOrcJITStackRef _string -> LLVMOrcTargetAddress))
(define-llvm LLVMOrcDisposeInstance (_fun LLVMOrcJITStackRef -> _void))
