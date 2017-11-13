#lang racket

(require "llvm/ffi/all.rkt")
(require "info-key.rkt"
         "env.rkt")
(provide (all-defined-out))

(define (jit-dump-module mod-env)
  (LLVMDumpModule (env-get-module mod-env)))

(define (jit-dump-function mod-env fsym)
  (LLVMDumpValue (env-jit-function-ref (env-lookup fsym mod-env))))

(define (jit-write-bitcode mod-env file-name)
  (LLVMWriteBitcodeToFile (env-get-module mod-env) file-name))

(define (jit-write-module mod-env file-name)
  (LLVMPrintModuleToFile (env-get-module mod-env) file-name))
