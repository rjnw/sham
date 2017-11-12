#lang racket

(define (jit-dump-module mod)
  (LLVMDumpModule (jit-get-module mod))
  (for [(m mod)]
    (let ([s (car m)]
          [v (cdr m)])
      (cond ([env-type? v])
             ;; (printf "type ~a: ~a\n" s
             ;;         (LLVMPrintTypeToString (internal-type-jit (env-type-prim v))))

            ([env-jit-function? v]
             (LLVMDumpValue (env-jit-function-ref v)))))))

(define (jit-dump-function mod sym)
  (LLVMDumpValue (env-jit-function-ref (env-lookup sym mod))))

(define (jit-write-bitcode mod fname)
  (LLVMWriteBitcodeToFile (jit-get-module mod) fname))
(define (jit-write-module mod fname)
  (LLVMPrintModuleToFile (jit-get-module mod) fname))
