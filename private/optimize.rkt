#lang racket
(require "llvm/ffi/all.rkt"
         "llvm/pass-table.rkt"
         "mod-env-info.rkt"
         "fun-info.rkt"
         "type-info.rkt"
         "env.rkt")

(provide optimize-module)

(define (optimize-module mod-env)
  (define all-function-info (env-get-info-key mod-env per-function-info-key))
  (map (curryr do-function-info mod-env) all-function-info)
  (define module-passes (env-get-info-key mod-env module-pass-info-key))
  (run-module-passes (env-get-module mod-env) module-passes))

(define (run-module-passes jit-mod passes)
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes])
    (define module-pass (lookup-pass pass))
    (module-pass mpm))
  (begin0
      (LLVMRunPassManager mpm jit-mod)
      (LLVMDisposePassManager mpm)))

(define (do-function-info fname-info mod-env)
  (match-define (cons fname finfo) fname-info)
  (define jit-mod (env-get-module mod-env))
  (define context (env-get-context mod-env))
  (define envf (env-lookup fname mod-env))
  (define lf (env-jit-function-ref envf))
  (define (add-attributes attrs index)
    (for ([attr attrs])
      (LLVMAddAttributeAtIndex lf  (lookup-attribute attr context))))

  (for ([(key val) (in-hash finfo)])
    (match key
      [fn-pass-key (run-function-pass lf jit-mod val)]
      ['fn (add-attributes val function-attribute-index)]
      ['ret (add-attributes val 0)]
      [`(arg ,n) (add-attributes val 0)]
      [else (printf "for now doing nothing for this key in function info" key)])))


(define (run-function-pass f jit-mod passes)
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (for ([pass passes])
    ((lookup-pass pass) fpm))
  (begin0
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref f))
    (LLVMDisposePassManager fpm)))

;; (define (jit-optimize-function mod-env #:opt-level [level 1])
;;   (define jit-mod (env-get-module mod-env))
;;   (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
;;   (define fpmb (LLVMPassManagerBuilderCreate))
;;   (LLVMPassManagerBuilderSetOptLevel fpmb level)
;;   (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
;;   (for [(m mod-env)]
;;     (when (env-jit-function? (cdr m))
;;       (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
;;   (LLVMDisposePassManager fpm)
;;   (LLVMPassManagerBuilderDispose fpmb))


;;we don't want rely on llvm's basic optimization levels anymore
;; as they are based on c style languages. need to figure out our own
;; (define (jit-optimize-module mod-env #:opt-level [level 1])
;;   (define jit-mod (env-get-module mod-env))
;;   (define mpm (LLVMCreatePassManager))
;;   (define pmb (LLVMPassManagerBuilderCreate))
;;   (LLVMPassManagerBuilderSetOptLevel pmb level)
;;   (LLVMPassManagerBuilderSetSizeLevel pmb 100)
;;   (LLVMPassManagerBuilderPopulateModulePassManager pmb mpm)
;;   (begin0
;;       (LLVMRunPassManager mpm jit-mod)
;;     (LLVMDisposePassManager mpm)
;;     (LLVMPassManagerBuilderDispose pmb)))
