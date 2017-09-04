#lang racket

(require ffi/unsafe
         "build-adjunct.rkt"
         "ffi/ctypes.rkt"
         "ffi/target-machine.rkt"
         "ffi/execution-engine.rkt")

(provide LLVMCreateMCJITCompileForModuleWithTarget
         LLVMAdjunctAddGlobalMapping)

(define LLVMCreateMCJITCompileForModuleWithTarget
  (get-ffi-obj "LLVMCreateMCJITCompilerForModuleWithTarget"
               adjunct-lib
               (_fun (ee : (_ptr o LLVMExecutionEngineRef))
                     (mod : LLVMModuleRef)
                     (tmr : LLVMTargetMachineRef)
                     (options : (_ptr i _LLVMMCJITCompilerOptions))
                     (err : (_ptr o _string))
                     -> (status : LLVMBool)
                     -> (values ee status err))))


(define LLVMAdjunctAddGlobalMapping
  (get-ffi-obj "LLVMAdjunctAddGlobalMapping"
               adjunct-lib
               (_fun (engine : LLVMExecutionEngineRef)
                     (fvalue : LLVMValueRef)
                     (libname : _string)
                     (fname : _string)
                     -> _void)))
