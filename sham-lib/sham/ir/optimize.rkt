#lang racket
(require sham/llvm
         sham/env)

(provide optimize-llvm-module)

;; TODO run specific pass defined in meta information
(define (optimize-llvm-module mod-env
                             #:opt-level [olevel 1]
                             #:size-level [slevel 1]
                             #:loop-vec [lvec #f]
                             #:slp-vec [svec #f]
                             )
  (LLVMCustomInitializeCL 1 '("sham"))
  ;; (printf "optimize-module: ~a\n" mod-env)

  (define finfo-map (env-get-per-function-info-map mod-env))
  (for ([(key val) (in-hash finfo-map)])
    (apply-function-info key val mod-env))
  (run-module-passes (env-get-early-module-passes mod-env) mod-env)

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

  ;; (add-module-info-passes module-pass-manager (env-get-module-info-map mod-env))
  (LLVMRunPassManager module-pass-manager llvm-module)
  (LLVMDisposePassManager module-pass-manager)

  (define function-pass-manager (LLVMCreateFunctionPassManagerForModule llvm-module))
  (LLVMPassManagerAddTargetLibraryInfoPass function-pass-manager llvm-module)
  (LLVMPassManagerAddTargetIRAnalysis function-pass-manager target-machine)
  (LLVMPassManagerBuilderPopulateFunctionPassManager pass-manager-builder function-pass-manager)
  (for [(m mod-env)]
    (when (env-function? (cdr m))
      (LLVMRunFunctionPassManager function-pass-manager (env-function-ref (cdr m)))))

  (LLVMDisposePassManager function-pass-manager)
  (LLVMPassManagerBuilderDispose pass-manager-builder)

  (run-module-passes (env-get-late-module-passes mod-env) mod-env))

(define (run-module-passes passes mod-env)
  (define llvm-mod (env-get-llvm-module mod-env))
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes])
    (define module-pass (lookup-pass pass))
    (module-pass mpm))
  (begin0
      (LLVMRunPassManager mpm llvm-mod)
    (LLVMDisposePassManager mpm)))

(define (apply-function-info fname finfo mod-env)
  (define llvm-mod (env-get-llvm-module mod-env))
  (define context (env-get-context mod-env))
  (define envf (env-lookup fname mod-env))
  (define lf (env-function-ref envf))
  (define (add-attributes attrs index)
    (for ([attr attrs])
      (LLVMAddAttributeAtIndex lf index (lookup-attribute attr context))))

  (when finfo
    (for ([(key val) (in-hash finfo)])
      (match key
        ['attribute (add-attributes val function-attribute-index)]
        ['ret (add-attributes val 0)]
        [`(arg ,n) (add-attributes val n)]
        ['pass (run-function-pass lf llvm-mod val)]
        ['call-conv (void)]
        [else (printf "for now doing nothing for this key in function info ~a." key)]))))

(define (run-function-pass f llvm-mod passes)
  (define fpm (LLVMCreateFunctionPassManagerForModule llvm-mod))
  (for ([pass passes])
    ((lookup-pass pass) fpm))
  (begin0
      (LLVMRunFunctionPassManager fpm (env-function-ref f))
    (LLVMDisposePassManager fpm)))

(define (basic-optimize-function mod-env #:opt-level [level 1])
  (define llvm-mod (env-get-llvm-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule llvm-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))
