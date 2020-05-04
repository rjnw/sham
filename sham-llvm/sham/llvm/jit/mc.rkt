#lang racket

(require sham/md
         sham/private/env
         sham/llvm/ffi/all
         sham/llvm/ir/ast
         sham/llvm/ir/env
         sham/llvm/jit/env)

(provide (all-defined-out))

(define (initialize-mcjit env #:opt-level [opt-level 1])
  (checked-initialize-llvm)
  (unless (llvm-env? env)
    (error 'sham:jit:mc "not a valid llvm-env ~a" env))
  (define mcjit-options (LLVMInitializeMCJITCompilerOptions))
  (set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options opt-level)
  (define-values (mcjit status err)
    (LLVMCreateMCJITCompilerForModule (llvm-env-module-ref env) mcjit-options))
  (when status
    (error 'sham:jit:mc "error initializing llvm mcjit ~a ~a" status err))
  (add-external-mappings env mcjit)
  (llvm-mcjit-env env mcjit))

(define (add-external-mappings env mcjit-ref)
  (match-define (llvm-env mref cref ast vrefs) env)
  (match-define (llvm:def:module md mid mdefs) ast)
  (define external-mappings (ref-module-md-jit-external-mappings md))
  (when external-mappings
    (for ([m external-mappings])
      (match-define (external-mapping name uintptr) m)
      (match-define (llvm-external value-ref type-ref) (llvm-env-lookup-value env name))
      (LLVMAddGlobalMapping mcjit-ref value-ref uintptr))))

(define (mcjit-function-address env fname)
  (match-define (llvm-mcjit-env lenv jit-ref) env)
  (LLVMGetFunctionAddress jit-ref (to-string fname)))
(define (mcjit-global-value-address env vname)
  (match-define (llvm-mcjit-env lenv jit-ref) env)
  (LLVMGetGlobalValueAddress jit-ref (to-string vname)))
