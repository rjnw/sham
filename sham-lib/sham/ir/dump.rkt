#lang racket

(require sham/llvm/ffi
         sham/env)
(provide (all-defined-out))

(define (dump-llvm-module mod-env)
  (LLVMDumpModule (env-get-llvm-module mod-env)))

(define (dump-llvm-function mod-env fsym)
  (LLVMDumpValue (env-function-ref (env-lookup fsym mod-env))))

(define (write-llvm-bitcode mod-env file-name)
  (LLVMWriteBitcodeToFile (env-get-llvm-module mod-env) file-name))

(define (write-llvm-module mod-env file-name)
  (match-define (vector error? error-message)
    (LLVMPrintModuleToFile (env-get-llvm-module mod-env) file-name))
  (when error?
    (error 'sham/write-module
           (format
            (string-join
            `("couldn't generate llvm module file: ~a"
              "LLVM: ~a")
            "\n")
            file-name
            error-message)))
  (void))
