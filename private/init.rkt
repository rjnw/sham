#lang racket

(require "llvm/ffi/all.rkt"
         "llvm/adjunct.rkt"
         "module-env.rkt"
         "internals.rkt"
         "types.rkt"
         "env.rkt"
         "rator.rkt")
(require ffi/unsafe)
(provide (all-defined-out))

(llvm-initialize-all)

(define (create-jit-context)
  (LLVMContextCreate))
(define global-jit-context (LLVMGetGlobalContext))
(define (create-initial-environment context)
  (register-jit-internals (register-initial-types (empty-env) context) context))

(define (initialize-jit! mod-env #:opt-level [opt-level 3])
  ;; hmm this is not affecting performance at all, weird
  (define mcjit-options (LLVMInitializeMCJITCompilerOptions))
  (set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options opt-level)
  (define-values (mcjit status err)
    (LLVMCreateMCJITCompilerForModule (env-get-module mod-env) mcjit-options))
  (if status
      (error "error initializing jit" status err)
      (begin
        (env-add-mcjit! mod-env mcjit)
        (add-ffi-mappings mod-env)
        (add-rkt-mappings mod-env)))
  (void))

(define (initialize-orc! mod-env)
  (define orc (LLVMOrcCreateInstance (LLVMCreateCurrentTargetMachineRef)))
  (define ms (LLVMOrcMakeSharedModule (env-get-module mod-env)))
  (define (null-symbol-resolver name orc) (printf "called symbol resolver\n") 42)
  (define-values (error-code module-handle)
    (LLVMOrcAddEagerlyCompiledIR orc ms (function-ptr null-symbol-resolver LLVMOrcSymbolResolverFn) orc))
  (LLVMOrcDisposeSharedModuleRef ms)
  (env-add-orc! mod-env orc))
