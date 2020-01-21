#lang racket

(require ffi/unsafe
         "build-adjunct.rkt"
         "ffi/ctypes.rkt"
         "ffi/target-machine.rkt"
         "ffi/execution-engine.rkt")

(provide (all-defined-out))

(define (get-adjunct-obj name alib type)
  (if alib
      (get-ffi-obj name alib type)
      (λ args (error 'sham (format "adjunct file not built, ~a not available" name)))))

(define LLVMCreateMCJITCompilerForModuleWithTarget
  (get-adjunct-obj "LLVMCreateMCJITCompilerForModuleWithTarget"
                   adjunct-lib
                   (_fun (ee : (_ptr o LLVMExecutionEngineRef))
                         (mod : LLVMModuleRef)
                         (options : (_ptr i _LLVMMCJITCompilerOptions))
                         (err : (_ptr o _string))
                         -> (status : LLVMBool)
                         -> (values ee status err))))


(define LLVMCreateCurrentTargetMachineRef
  (get-adjunct-obj "LLVMCreateCurrentTargetMachineRef"
                   adjunct-lib
                   (_fun -> LLVMTargetMachineRef)))

(define LLVMCustomInitializeCL
  (get-adjunct-obj "LLVMCustomInitializeCL"
                   adjunct-lib
                   (_fun _int (_list i _string) -> _void)))

(define LLVMPassManagerBuilderSetLoopVectorize
  (get-adjunct-obj "LLVMPassManagerBuilderSetLoopVectorize"
                   adjunct-lib
                   (_fun LLVMPassManagerBuilderRef LLVMBool -> _void)))

(define LLVMPassManagerBuilderSetSLPVectorize
  (get-adjunct-obj "LLVMPassManagerBuilderSetSLPVectorize"
                   adjunct-lib
                   (_fun LLVMPassManagerBuilderRef LLVMBool -> _void)))

(define LLVMPassManagerBuilderSetInliner
  (get-adjunct-obj "LLVMPassManagerBuilderSetInliner"
                   adjunct-lib
                   (_fun LLVMPassManagerBuilderRef _uint _uint -> _void)))

(define LLVMTargetMachineAdjustPassManagerBuilder
  (get-adjunct-obj "LLVMTargetMachineAdjustPassManagerBuilder"
                   adjunct-lib
                   (_fun LLVMPassManagerBuilderRef LLVMTargetMachineRef -> _void)))

(define LLVMPassManagerAddTargetLibraryInfoPass
  (get-adjunct-obj "LLVMPassManagerAddTargetLibraryInfoPass"
                   adjunct-lib
                   (_fun LLVMPassManagerRef LLVMModuleRef -> _void)))

(define LLVMPassManagerAddTargetIRAnalysis
  (get-adjunct-obj "LLVMPassManagerAddTargetIRAnalysis"
                   adjunct-lib
                   (_fun LLVMPassManagerRef LLVMTargetMachineRef -> _void)))
