#lang racket
(require "../main.rkt"
         "../private/ast-utils.rkt")
(require "../private/llvm/ffi/all.rkt")

(define mod1 (create-empty-sham-module "global-module"))

(define-sham-function #:module mod1
    (pow (x : i32) (n : i32)) : i32
    (if^ (icmp-ule n (ui32 0))
         (return (ui32 1))
         (return (mul x (pow x (sub-nuw n (ui32 1)))))))


(compile-sham-module! mod1)

(llvm-app pow 2 2)


;; (module+ test
;;   (define-module mod1-ast
;;     (empty-mod-env-info)
;;     (list pow))
;;   (define mod1 (compile-sham-module mod1-ast))
;;   (define p (jit-get-function mod1-ast 'pow))



;;   (define lmod1 (env-get-llvm-module mod1))

;;   (define-module mod2-ast
;;     (empty-mod-env-info)
;;     (list pow2))

;;   (define mod2 (jit-module-compile mod2-ast))
;;   (define lmod2 (env-get-llvm-module mod2))
;;   (define lmod1c (LLVMCloneModule lmod1))

;;   (define mod21c (LLVMLinkModules2 lmod2 lmod1c)) ;; destroys mod1c

;;   (define orc2 (create-orc-instance))
;;   (define mod2h (orc-add-lazy-llvm-module! orc2 lmod2))

;;   (orc-get-function-address orc2 'pow)


;;   (define-module mod3-ast
;;     (empty-mod-env-info)
;;     (list pow))
;;   (define mod3 (jit-module-compile mod3-ast))
;;   (define mod3^ (jit-module-add-function mod3 (get-function-for-module pow2)))

;;   (jit-verify-module mod3^)
;;   (module-initialize-orc! mod3^)
;;   (define pp (jit-get-function 'pow2 mod3^)))
