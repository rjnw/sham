#lang racket
(require "define.rkt"
         "ctypes.rkt")
(require ffi/unsafe)
(provide (all-defined-out))

(define-llvm LLVMInitializeTransformUtils (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeScalarOpts (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeObjCARCOpts (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeVectorization (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeInstCombine (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeIPO (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeInstrumentation (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeAnalysis (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeIPA (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeCodeGen (_fun LLVMPassRegistryRef -> _void))
(define-llvm LLVMInitializeTarget (_fun LLVMPassRegistryRef -> _void))
