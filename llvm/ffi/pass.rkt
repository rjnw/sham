#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))



;/*===-- Pass Registry -----------------------------------------------------===*/

;/** Return the global pass registry, for use with initialization functions.
;    See llvm::PassRegistry::getPassRegistry. */
(define-llvm LLVMGetGlobalPassRegistry
 (_fun -> LLVMPassRegistryRef))


;/*===-- Pass Managers -----------------------------------------------------===*/

;/** Constructs a new whole-module pass pipeline. This type of pipeline is
;    suitable for link-time optimization and whole-module transformations.
;    See llvm::PassManager::PassManager. */
(define-llvm LLVMCreatePassManager (_fun -> LLVMPassManagerRef))

;/** Constructs a new function-by-function pass pipeline over the module
;    provider. It does not take ownership of the module provider. This type of
;    pipeline is suitable for code generation and JIT compilation tasks.
;    See llvm::FunctionPassManager::FunctionPassManager. */
(define-llvm LLVMCreateFunctionPassManagerForModule (_fun LLVMModuleRef -> LLVMPassManagerRef))


;/** Initializes, executes on the provided module, and finalizes all of the
;    passes scheduled in the pass manager. Returns 1 if any of the passes
;    modified the module, 0 otherwise. See llvm::PassManager::run(Module&). */
(define-llvm LLVMRunPassManager (_fun LLVMPassManagerRef LLVMModuleRef -> LLVMBool))

;/** Initializes all of the function passes scheduled in the function pass
;    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
;    See llvm::FunctionPassManager::doInitialization. */
(define-llvm LLVMInitializeFunctionPassManager (_fun LLVMPassManagerRef -> LLVMBool))

;/** Executes all of the function passes scheduled in the function pass manager
;    on the provided function. Returns 1 if any of the passes modified the
;    function, false otherwise.
;    See llvm::FunctionPassManager::run(Function&). */
(define-llvm LLVMRunFunctionPassManager (_fun LLVMPassManagerRef LLVMValueRef -> LLVMBool))

;/** Finalizes all of the function passes scheduled in in the function pass
;    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
;    See llvm::FunctionPassManager::doFinalization. */
(define-llvm LLVMFinalizeFunctionPassManager (_fun LLVMPassManagerRef -> LLVMBool))

;/** Frees the memory of a pass pipeline. For function pipelines, does not free
;    the module provider.
;    See llvm::PassManagerBase::~PassManagerBase. */
(define-llvm LLVMDisposePassManager (_fun LLVMPassManagerRef -> _void))
