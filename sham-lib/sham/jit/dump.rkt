#lang racket

(require sham/llvm/ffi/all)
(require "module-env.rkt"
         "env.rkt")
(provide (all-defined-out))

(define (jit-dump-module mod-env)
  (LLVMDumpModule (env-get-llvm-module mod-env)))

(define (jit-dump-function mod-env fsym)
  (LLVMDumpValue (env-jit-function-ref (env-lookup fsym mod-env))))

(define (jit-write-bitcode mod-env file-name)
  (LLVMWriteBitcodeToFile (env-get-llvm-module mod-env) file-name))

(define (jit-write-module mod-env file-name)
  (match-define (vector error? error-message)
    (LLVMPrintModuleToFile (env-get-llvm-module mod-env) file-name))
  (when error?
    (error 'sham/jit-write-module
           (format
            (string-join
            `("couldn't generate llvm module file: ~a"
              "LLVM: ~a")
            "\n")
            file-name
            error-message)))
  (void))
