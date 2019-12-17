#lang racket
(require "define.rkt"
         "ctypes.rkt"
         ffi/unsafe
         ffi/unsafe/define)
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
(define-llvm LLVMGetFirstTarget (_fun -> LLVMTargetRef))
(define-llvm LLVMGetNextTarget (_fun LLVMTargetRef -> LLVMTargetRef))
(define-llvm LLVMGetTargetFromName (_fun _string -> LLVMTargetRef))
(define-llvm LLVMGetTargetFromTriple
  (_fun _string (t : (_ptr o LLVMTargetRef)) (e : (_ptr o _string))
        -> (fail : LLVMBool)
        -> (values t e fail)))

(define-llvm LLVMGetTargetName (_fun LLVMTargetRef -> _string))
(define-llvm LLVMGetTargetDescription (_fun LLVMTargetRef -> _string))
(define-llvm LLVMTargetHasJIT (_fun LLVMTargetRef -> LLVMBool))
(define-llvm LLVMTargetHasTargetMachine (_fun LLVMTargetRef -> LLVMBool))
(define-llvm LLVMTargetHasAsmBackend (_fun LLVMTargetRef -> LLVMBool))
(define-llvm LLVMDisposeTargetMachine (_fun LLVMTargetMachineRef -> _void)
)
(define-llvm LLVMCreateTargetMachine
  (_fun LLVMTargetRef _string _string _string LLVMCodeGenOptLevel LLVMRelocMode LLVMCodeModel
        -> LLVMTargetMachineRef))

(define-llvm LLVMGetTargetMachineTarget (_fun LLVMTargetMachineRef -> LLVMTargetRef))
(define-llvm LLVMGetTargetMachineTriple (_fun LLVMTargetMachineRef -> _string))
(define-llvm LLVMGetTargetMachineCPU (_fun LLVMTargetMachineRef -> _string))
(define-llvm LLVMGetTargetMachineFeatureString (_fun LLVMTargetMachineRef -> _string))
(define-llvm LLVMCreateTargetDataLayout (_fun LLVMTargetMachineRef -> LLVMTargetDataRef))
(define-llvm LLVMSetTargetMachineAsmVerbosity (_fun LLVMTargetMachineRef LLVMBool -> _void))
(define-llvm LLVMTargetMachineEmitToFile
  (_fun LLVMTargetMachineRef LLVMModuleRef _string LLVMCodeGenFileType _string -> LLVMBool))
(define-llvm LLVMTargetMachineEmitToMemoryBuffer
  (_fun LLVMTargetMachineRef LLVMModuleRef LLVMCodeGenFileType _pointer (pointer-to LLVMMemoryBufferRef) -> LLVMBool))
;disposemessage
(define-llvm LLVMGetDefaultTargetTriple (_fun -> _string))
(define-llvm LLVMAddAnalysisPasses (_fun LLVMTargetMachineRef LLVMPassManagerRef -> _void))
