#lang racket
(require "llvm/ffi/all.rkt"
         "llvm/pass-table.rkt"
         "llvm/adjunct.rkt"
         "mod-env-info.rkt"
         "fun-info.rkt"
         "type-info.rkt"
         "env.rkt")

(provide optimize-module)

(define (optimize-module mod-env)
  (define all-function-info (env-get-info-key mod-env per-function-info-key))
  (for ([(key val) (in-hash all-function-info)])
    (do-function-info key val mod-env))
  (LLVMRunOurModulePasses (env-get-module mod-env)))
  ;(basic-optimize-module mod-env))



(define (run-module-passes jit-mod passes)
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes])
    (define module-pass (lookup-pass pass))
    (module-pass mpm))
  (begin0
      (LLVMRunPassManager mpm jit-mod)
      (LLVMDisposePassManager mpm)))

(define (do-function-info fname finfo mod-env)
  (define jit-mod (env-get-module mod-env))
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
  (define jit-mod (env-get-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))


;;we don't want rely on llvm's basic optimization levels anymore
;; as they are based on c style languages. need to figure out our own
(define (basic-optimize-module mod-env #:opt-level [level 3])
  (define jit-mod (env-get-module mod-env))
  (define mpm (LLVMCreatePassManager))
  (define pmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel pmb level)
  (LLVMPassManagerBuilderSetSizeLevel pmb 100)
  (LLVMPassManagerBuilderPopulateModulePassManager pmb mpm)
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)
    (LLVMPassManagerBuilderDispose pmb)))
