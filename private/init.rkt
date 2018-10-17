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

(define (create-jit-context) (LLVMContextCreate))
(define global-jit-context (make-parameter (LLVMGetGlobalContext)))

(define (create-initial-environment context)
  (register-jit-internals (register-initial-types (empty-env) context) context))
(define full-data-layout
  (string-join
   '("e"
     "m:e"
     "p:64:64:64"
     "i1:8:8"
     "i8:8:8"
     "i16:16:16"
     "i32:32:32"
     "i64:64:64"
     ;;integer types alignment
     "f32:32:32"
     "f64:64:64"
     "f128:128:128" ;;floating types alignment
     "v64:64:64"
     "v128:128:128" ;; vector type alignment
     "a:0:64"
     "s0:64:64"
     "n8:16:32:64" ;; a aggregate alignment
     "S128"        ;stack alignment
     )
   "-"))


(define (initialize-jit! mod-env #:opt-level [opt-level 3])
  ;; hmm this is not affecting performance at all, weird
  (define mcjit-options (LLVMInitializeMCJITCompilerOptions))
  (set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options opt-level)
  (define-values (mcjit status err)
    (LLVMCreateMCJITCompilerForModule (env-get-llvm-module mod-env) mcjit-options))
  (if status
      (error "error initializing jit" status err)
      (begin
        (env-add-mcjit! mod-env mcjit)
        (add-ffi-mappings mod-env)
        (add-rkt-mappings mod-env)))
  (void))

(define (null-symbol-resolver name orc)
  (printf "orc called symbol resolver, should not happen!\n") 0)

(define (module-initialize-orc! mod-env)
  (define orc (create-orc-instance))
  (define orc-handle (orc-add-eager-module! orc mod-env))
  (env-add-orc-handle! mod-env orc-handle)
  (env-add-orc! mod-env orc))

(define (create-orc-instance)
  (LLVMOrcCreateInstance (LLVMCreateCurrentTargetMachineRef)))

(define (orc-add-lazy-llvm-module! orc mod)
  (define-values (error-code module-handle)
    (LLVMOrcAddLazilyCompiledIR orc mod null-symbol-resolver orc))
  (if (equal? error-code 'LLVMOrcErrSuccess)
      module-handle
      (error "orc-add-lazy-module! error" error-code)))
(define (orc-add-eager-llvm-module! orc mod)
  (define-values (error-code module-handle)
    (LLVMOrcAddEagerlyCompiledIR orc mod null-symbol-resolver orc))
  (if (equal? error-code 'LLVMOrcErrSuccess)
      module-handle
      (error "orc-add-eager-module! error" error-code)))
(define (orc-add-lazy-module! orc mod-env)
  (orc-add-lazy-llvm-module! orc (env-get-llvm-module mod-env)))
(define (orc-add-eager-module! orc mod-env)
  (orc-add-eager-llvm-module! orc (env-get-llvm-module mod-env)))
(define (orc-get-function-address orc fsym)
  (LLVMOrcGetSymbolAddress orc (symbol->string fsym)))
(define (orc-get-function-pointer orc fsym)
  (cast (orc-get-function-address orc fsym) _uint64 _pointer))
