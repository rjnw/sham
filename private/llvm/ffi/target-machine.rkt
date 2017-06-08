#lang racket
(require "define.rkt"
         "ctypes.rkt")
(require ffi/unsafe)
(provide (all-defined-out))

(define-llvm-types
  LLVMTargetRef
  LLVMTargetMachineRef)

(define LLVMCodeGenOptLevel
  (_enum
   '(LLVMCodeGenLevelNone
     LLVMCodeGenLevelLess
     LLVMCodeGenLevelDefault
     LLVMCodeGenLevelAggressive)))

(define LLVMRelocMode
  (_enum
   '(LLVMRelocDefault
     LLVMRelocStatic
     LLVMRelocPIC
     LLVMRelocDynamicNoPic)))

(define LLVMCodeModel
  (_enum
   '(LLVMCodeModelDefault 	
     LLVMCodeModelJITDefault 	
     LLVMCodeModelSmall 	
     LLVMCodeModelKernel 	
     LLVMCodeModelMedium 	
     LLVMCodeModelLarge)))

(define LLVMCodeGenFileType
  (_enum
   '(LLVMAssemblyFile
     LLVMObjectFile)))


