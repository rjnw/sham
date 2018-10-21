#lang racket
(require "llvm/ffi/all.rkt"
         "llvm/pass-table.rkt"
         "llvm/adjunct.rkt"
         "module-env.rkt"
         "info.rkt"
         "type-info.rkt"
         "env.rkt")

(provide jit-optimize-module)

;; TODO run specific pass defined in meta information
(define (jit-optimize-module mod-env
                         #:opt-level [olevel 1]
                         #:size-level [slevel 1]
                         #:loop-vec [lvec #f]
                         #:slp-vec [svec #f]
                         )
  (LLVMCustomInitializeCL 1 '("sham-jit"))
  (define llvm-module (env-get-llvm-module mod-env))
  (define module-pass-manager (LLVMCreatePassManager))
  (define pass-manager-builder (LLVMPassManagerBuilderCreate))
  (define target-machine (LLVMCreateCurrentTargetMachineRef))
  (LLVMTargetMachineAdjustPassManagerBuilder pass-manager-builder target-machine)
  (LLVMPassManagerBuilderSetOptLevel pass-manager-builder olevel)
  (LLVMPassManagerBuilderSetSizeLevel pass-manager-builder slevel)
  (LLVMPassManagerBuilderSetSLPVectorize pass-manager-builder lvec)
  (LLVMPassManagerBuilderSetLoopVectorize pass-manager-builder svec)

  (LLVMPassManagerAddTargetLibraryInfoPass module-pass-manager llvm-module)
  (LLVMPassManagerAddTargetIRAnalysis module-pass-manager target-machine)
  (LLVMPassManagerBuilderPopulateModulePassManager pass-manager-builder module-pass-manager)

  (LLVMRunPassManager module-pass-manager llvm-module)
  (LLVMDisposePassManager module-pass-manager)

  (define function-pass-manager (LLVMCreateFunctionPassManagerForModule llvm-module))
  (LLVMPassManagerAddTargetLibraryInfoPass function-pass-manager llvm-module)
  (LLVMPassManagerAddTargetIRAnalysis function-pass-manager target-machine)
  (LLVMPassManagerBuilderPopulateFunctionPassManager pass-manager-builder function-pass-manager)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager function-pass-manager (env-jit-function-ref (cdr m)))))

  (LLVMDisposePassManager function-pass-manager)
  (LLVMPassManagerBuilderDispose pass-manager-builder))

(define (run-module-passes jit-mod passes)
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes])
    (define module-pass (lookup-pass pass))
    (module-pass mpm))
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)))

(define (do-function-info fname finfo mod-env)
  (define jit-mod (env-get-llvm-module mod-env))
  (define context (env-get-context mod-env))
  (define envf (env-lookup fname mod-env))
  (define lf (env-jit-function-ref envf))
  (define (add-attributes attrs index)
    (for ([attr attrs])
      (LLVMAddAttributeAtIndex lf index (lookup-attribute attr context))))

  (for ([(key val) (in-hash finfo)])
    (match key
      ['fn (add-attributes val function-attribute-index)]
      ['ret (add-attributes val 0)]
      [`(arg ,n) (add-attributes val n)]
      ['pass (run-function-pass lf jit-mod val)]
      [else (printf "for now doing nothing for this key in function info" key)])))

(define (run-function-pass f jit-mod passes)
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (for ([pass passes])
    ((lookup-pass pass) fpm))
  (begin0
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref f))
    (LLVMDisposePassManager fpm)))

(define (basic-optimize-function mod-env #:opt-level [level 1])
  (define jit-mod (env-get-llvm-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))
