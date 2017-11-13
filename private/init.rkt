#lang racket

(require "llvm/ffi/all.rkt"
         "llvm/adjunct.rkt"
         "info-key.rkt"
         "internals.rkt"
         "types.rkt"
         "env.rkt"
         "rator.rkt")
(provide (all-defined-out))

(define (llvm-initialize)
  (LLVMLinkInMCJIT)
  (LLVMInitializeX86Target)
  (LLVMInitializeX86TargetInfo)
  (LLVMInitializeX86TargetMC)
  (LLVMInitializeX86AsmParser)
  (LLVMInitializeX86AsmPrinter)
  (define gpr (LLVMGetGlobalPassRegistry))
  (LLVMInitializeCore gpr)
  (LLVMInitializeTransformUtils gpr)
  (LLVMInitializeScalarOpts gpr)
  (LLVMInitializeObjCARCOpts gpr)
  (LLVMInitializeVectorization gpr)
  (LLVMInitializeInstCombine gpr)
  (LLVMInitializeIPO gpr)
  (LLVMInitializeInstrumentation gpr)
  (LLVMInitializeAnalysis gpr)
  (LLVMInitializeIPA gpr)
  (LLVMInitializeCodeGen gpr)
  (LLVMInitializeTarget gpr))
(llvm-initialize)

(define (create-jit-context)
  (LLVMContextCreate))
(define global-jit-context (LLVMGetGlobalContext))
(define (create-initial-environment context)
  (register-jit-internals (register-initial-types (empty-env) context) context))

(define (initialize-jit! mod-env #:opt-level [opt-level 1])
  (printf "mod-env: ~a\n" mod-env)
  (printf "env-info: ~a\n" (env-get-info mod-env))
  (printf "jit-module: ~a\n" (env-get-module mod-env))
  (define mcjit-options (LLVMInitializeMCJITCompilerOptions))
  (set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options opt-level)
  (define-values (mcjit status err)
    (LLVMCreateMCJITCompilerForModuleWithTarget (env-get-module mod-env) mcjit-options))
  (if status
      (error "error initializing jit" status err)
      (begin
        (env-add-mcjit! mod-env mcjit)
        (add-ffi-mappings mod-env)
        (add-rkt-mappings mod-env)))
  (void))

  ;; (initialize-orc-jit mod-env)

(define (initialize-orc-jit mod-env)
  (define orc (LLVMOrcCreateInstance (LLVMCreateCurrentTargetMachineRef)))
  (env-add-orc! mod-env orc))
;; (LLVMOrcAddEagerlyCompiledIR orc (jit-get-module mod-env) symbolResolver orc)
