#lang racket
(require "define.rkt"
         "ctypes.rkt")
(require ffi/unsafe)

(define-llvm-types LLVMGenericValueRef
  LLVMExecutionEngineRef
  LLVMTargetDataRef
  LLVMTargetMachineRef
  LLVMMCJITMemoryManagerRef)
(define-llvm LLVMLinkInMCJIT (_fun -> _void))
(define-llvm LLVMLinkInInterpreter (_fun -> _void))
(define-llvm LLVMCreateGenericValueOfInt (_fun LLVMTypeRef _ullong LLVMBool -> LLVMGenericValueRef))
(define-llvm LLVMCreateGenericValueOfPointer (_fun _pointer -> LLVMGenericValueRef))
(define-llvm LLVMCreateGenericValueOfFloat (_fun LLVMTypeRef _double -> LLVMGenericValueRef))
(define-llvm LLVMGenericValueIntWidth (_fun LLVMGenericValueRef -> _uint))
(define-llvm LLVMGenericValueToInt (_fun LLVMGenericValueRef LLVMBool -> _ullong))
(define-llvm LLVMGenericValueToPointer (_fun LLVMGenericValueRef -> _pointer))
(define-llvm LLVMGenericValueToFloat (_fun LLVMTypeRef LLVMGenericValueRef -> _double))
(define-llvm LLVMDisposeGenericValue (_fun LLVMGenericValueRef -> _void))
;; (define-llvm LLVMCreateExecutionEngineForModule (_fun (pointer-to LLVMExecutionEngineRef) LLVMModuleRef _string -> LLVMBool))
;; (define-llvm LLVMCreateInterpreterForModule (_fun (pointer-to LLVMExecutionEngineRef) LLVMModuleRef _string -> LLVMBool))
;; (define-llvm LLVMCreateJITCompilerForModule (_fun (pointer-to LLVMExecutionEngineRef) LLVMModuleRef unsigned _string -> LLVMBool))
;; first argument is of type struct LLVMMCJITCompilerOptions
(define-llvm LLVMInitializeMCJITCompilerOptions (_fun _pointer _size -> _void))
;; (define-llvm LLVMCreateMCJITCompilerForModule (_fun (pointer-to LLVMExecutionEngineRef) LLVMModuleRef (pointer-to _pointer) _size _string -> LLVMBool))
(define-llvm LLVMDisposeExecutionEngine (_fun LLVMExecutionEngineRef -> _void))
(define-llvm LLVMRunStaticConstructors (_fun LLVMExecutionEngineRef -> _void))
(define-llvm LLVMRunStaticDestructors (_fun LLVMExecutionEngineRef -> _void))
;; (define-llvm LLVMRunFunctionAsMain (_fun LLVMExecutionEngineRef LLVMValueRef unsigned (pointer-to unknowntype) (pointer-to unknowntype) -> _int))
(define-llvm LLVMRunFunction (_fun LLVMExecutionEngineRef LLVMValueRef [_uint = (length args)] [args : (_list i LLVMGenericValueRef)] -> LLVMGenericValueRef))
(define-llvm LLVMFreeMachineCodeForFunction (_fun LLVMExecutionEngineRef LLVMValueRef -> _void))
(define-llvm LLVMAddModule (_fun LLVMExecutionEngineRef LLVMModuleRef -> _void))
;; (define-llvm LLVMRemoveModule (_fun LLVMExecutionEngineRef LLVMModuleRef (pointer-to LLVMModuleRef) _string -> LLVMBool))
;; (define-llvm LLVMFindFunction (_fun LLVMExecutionEngineRef _string (pointer-to LLVMValueRef) -> LLVMBool))
(define-llvm LLVMRecompileAndRelinkFunction (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))
(define-llvm LLVMGetExecutionEngineTargetData (_fun LLVMExecutionEngineRef -> LLVMTargetDataRef))
(define-llvm LLVMGetExecutionEngineTargetMachine (_fun LLVMExecutionEngineRef -> LLVMTargetMachineRef))
(define-llvm LLVMAddGlobalMapping (_fun LLVMExecutionEngineRef LLVMValueRef _pointer -> _void))
(define-llvm LLVMGetPointerToGlobal (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))
(define-llvm LLVMGetGlobalValueAddress (_fun LLVMExecutionEngineRef _string -> _uint64))
(define-llvm LLVMGetFunctionAddress (_fun LLVMExecutionEngineRef _string -> _uint64))
;; (define-llvm
;;   LLVMCreateSimpleMCJITMemoryManager
;;   (_fun
;;    _pointer
;;    LLVMMemoryManagerAllocateCodeSectionCallback
;;    LLVMMemoryManagerAllocateDataSectionCallback
;;    LLVMMemoryManagerFinalizeMemoryCallback
;;    LLVMMemoryManagerDestroyCallback
;;    ->
;;    LLVMMCJITMemoryManagerRef))
(define-llvm LLVMDisposeMCJITMemoryManager (_fun LLVMMCJITMemoryManagerRef -> _void))
