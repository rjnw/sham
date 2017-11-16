#lang racket

(require ffi/unsafe
         "build-adjunct.rkt"
         "ffi/ctypes.rkt"
         "ffi/target-machine.rkt"
         "ffi/execution-engine.rkt")

(provide LLVMCreateMCJITCompilerForModuleWithTarget
         LLVMCreateCurrentTargetMachineRef
         LLVMRunOurModulePasses)

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
;; (define LLVMRunOurModulePasses
;;   (get-ffi-obj "LLVMRunOurModulePasses"
;;                adjunct-lib
;;                (_fun LLVMModuleRef -> LLVMBool)))

(define (LLVMRunOurModulePasses x) #f)
