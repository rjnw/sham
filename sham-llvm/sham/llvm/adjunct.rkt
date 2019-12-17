#lang racket

(require ffi/unsafe
         "build-adjunct.rkt"
         "ffi/ctypes.rkt"
         "ffi/target-machine.rkt"
         "ffi/execution-engine.rkt")

(provide (all-defined-out))

(define LLVMCreateMCJITCompilerForModuleWithTarget
  (get-ffi-obj "LLVMCreateMCJITCompilerForModuleWithTarget"
               adjunct-lib
               (_fun (ee : (_ptr o LLVMExecutionEngineRef))
                     (mod : LLVMModuleRef)
                     (options : (_ptr i _LLVMMCJITCompilerOptions))
                     (err : (_ptr o _string))
                     -> (status : LLVMBool)
                     -> (values ee status err))))


(define LLVMCreateCurrentTargetMachineRef
  (get-ffi-obj "LLVMCreateCurrentTargetMachineRef"
               adjunct-lib
               (_fun -> LLVMTargetMachineRef)))

(define LLVMCustomInitializeCL
  (get-ffi-obj "LLVMCustomInitializeCL"
               adjunct-lib
               (_fun _int (_list i _string) -> _void)))

(define LLVMPassManagerBuilderSetLoopVectorize
  (get-ffi-obj "LLVMPassManagerBuilderSetLoopVectorize"
               adjunct-lib
               (_fun LLVMPassManagerBuilderRef LLVMBool -> _void)))

(define LLVMPassManagerBuilderSetSLPVectorize
  (get-ffi-obj "LLVMPassManagerBuilderSetSLPVectorize"
               adjunct-lib
               (_fun LLVMPassManagerBuilderRef LLVMBool -> _void)))

(define LLVMPassManagerBuilderSetInliner
  (get-ffi-obj "LLVMPassManagerBuilderSetInliner"
               adjunct-lib
               (_fun LLVMPassManagerBuilderRef _uint _uint -> _void)))

(define LLVMTargetMachineAdjustPassManagerBuilder
  (get-ffi-obj "LLVMTargetMachineAdjustPassManagerBuilder"
               adjunct-lib
               (_fun LLVMPassManagerBuilderRef LLVMTargetMachineRef -> _void)))

(define LLVMPassManagerAddTargetLibraryInfoPass
  (get-ffi-obj "LLVMPassManagerAddTargetLibraryInfoPass"
               adjunct-lib
               (_fun LLVMPassManagerRef LLVMModuleRef -> _void)))

(define LLVMPassManagerAddTargetIRAnalysis
  (get-ffi-obj "LLVMPassManagerAddTargetIRAnalysis"
               adjunct-lib
               (_fun LLVMPassManagerRef LLVMTargetMachineRef -> _void)))
