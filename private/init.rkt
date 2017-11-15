#lang racket

(require "llvm/ffi/all.rkt"
         "llvm/adjunct.rkt"
         "mod-env-info.rkt"
         "internals.rkt"
         "types.rkt"
         "env.rkt"
         "rator.rkt")
(provide (all-defined-out))

(llvm-initialize-all)

(define (create-jit-context)
  (LLVMContextCreate))
(define global-jit-context (LLVMGetGlobalContext))
(define (create-initial-environment context)
  (register-jit-internals (register-initial-types (empty-env) context) context))

(define (initialize-jit! mod-env #:opt-level [opt-level 1]) ;; hmm this is not affecting performance at all, weird
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

(define (initialize-orc-jit mod-env)
  (define orc (LLVMOrcCreateInstance (LLVMCreateCurrentTargetMachineRef)))
  (env-add-orc! mod-env orc))
;; (LLVMOrcAddEagerlyCompiledIR orc (jit-get-module mod-env) symbolResolver orc)
