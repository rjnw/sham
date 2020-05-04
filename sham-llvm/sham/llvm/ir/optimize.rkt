#lang racket
(require sham/llvm
         sham/md
         sham/llvm/ir/ast
         sham/llvm/ir/env)

(provide optimize-llvm-module!)

;; TODO run specific pass defined in meta information
(define (optimize-llvm-module! l-env
                               #:opt-level [olevel 1]
                               #:size-level [slevel 1]
                               #:loop-vec [lvec #f]
                               #:slp-vec [svec #f]
                               )
  (LLVMCustomInitializeCL 1 '("sham"))
  ;; (printf "optimize-module: ~a\n" l-env)
  (match-define (llvm-env llvm-module llvm-context l-ast v-refs) l-env)
  (define md (llvm-metadata l-ast))

  (for ([def (llvm:def:module-defs l-ast)])
    (when (llvm:def:function? def)
      (define def-id (llvm:def-id def))
      (define value (hash-ref v-refs def-id))
      (apply-function-md! value (llvm-metadata def) l-env)))

  (when-module-md-llvm-before-passes
   md before-passes
   (run-module-passes! before-passes l-env))

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

  ;; (add-module-info-passes module-pass-manager (env-get-module-info-map l-env))
  (LLVMRunPassManager module-pass-manager llvm-module)
  (LLVMDisposePassManager module-pass-manager)

  (define function-pass-manager (LLVMCreateFunctionPassManagerForModule llvm-module))
  (LLVMPassManagerAddTargetLibraryInfoPass function-pass-manager llvm-module)
  (LLVMPassManagerAddTargetIRAnalysis function-pass-manager target-machine)
  (LLVMPassManagerBuilderPopulateFunctionPassManager pass-manager-builder function-pass-manager)
  (for [(v (llvm-env-values l-env))]
    (when (llvm-function? v)
      (LLVMRunFunctionPassManager function-pass-manager (llvm-value-ref v))))

  (LLVMDisposePassManager function-pass-manager)
  (LLVMPassManagerBuilderDispose pass-manager-builder)

  (when-module-md-llvm-after-passes
   md after-passes
   (run-module-passes! after-passes l-env))
  l-env)

(define (run-module-passes! passes env)
  (define llvm-mod (llvm-env-module-ref env))
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes]) ((lookup-pass pass) mpm))
  (LLVMRunPassManager mpm llvm-mod))

(define (apply-function-md! fvalue fmd mod-env)
  (define general-attribute-index (modulo -1 (expt 2 32)))
  (define llvm-mod (llvm-env-module-ref mod-env))
  (define context (llvm-env-context-ref mod-env))
  (match-define (llvm-function llvm-ref l-type) fvalue)
  (define (add-attributes attrs index)
    (for ([attr attrs])
      (LLVMAddAttributeAtIndex llvm-ref index (lookup-attribute attr context))))
  (define (add-argument-attribute a-md)
    (match-define (cons argument-id attributes) a-md)
    (add-attributes attributes argument-id))
  (when fmd
    (when-function-md-llvm-general-attributes fmd gas (add-attributes gas general-attribute-index))
    (when-function-md-llvm-return-attributes fmd ras (add-attributes ras 0))
    (when-function-md-llvm-argument-attributes fmd aas (map add-argument-attribute aas))
    (when-function-md-llvm-optimization-pass fmd ps (run-function-passes! fvalue mod-env ps))))

(define (run-function-passes! f llvm-mod passes)
  (define fpm (LLVMCreateFunctionPassManagerForModule llvm-mod))
  (for ([pass passes]) ((lookup-pass pass) fpm))
  (LLVMRunFunctionPassManager fpm (llvm-value-ref f)))

(define (basic-optimize-all-functions mod-env #:opt-level [level 1])
  (define llvm-mod (llvm-env-module-ref mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule llvm-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(v (llvm-env-values mod-env))]
    (when (llvm-function? v)
      (LLVMRunFunctionPassManager fpm (llvm-value-ref v))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))

(define (basic-optimize-function l-env fname #:opt-level [level 1])
  (define llvm-mod (llvm-env-module-ref l-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule llvm-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (match-define (llvm-function value-ref type-ref) (llvm-env-lookup-value l-env fname))
  (LLVMRunFunctionPassManager fpm value-ref)
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))
