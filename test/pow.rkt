#lang racket
(require "../main.rkt"
         "../private/ast-utils.rkt"
         "../private/jit-utils.rkt"
         "../private/info.rkt")
(require "../private/llvm/ffi/all.rkt")

(define mod1
  (create-empty-sham-module "global-module"
                            (module-info-add-late-pass (empty-module-info) 'AlwaysInliner)))
(current-sham-module mod1)

(define-sham-function
  ;; #:info (function-info-add-attributes (empty-function-info) 'alwaysinline)
  (pow (x : i64) (n : i64)) : i64
  (if^ (icmp-ule n (ui64 0))
       (return (ui64 1))
       (return (mul x (pow x (sub-nuw n (ui64 1)))))))

(define to-check (make-parameter 'inline))
(define-sham-function
  (pow5 (x : i64)) : i64
  (return
   (match (to-check)
     ['normal (pow x (ui64 5))]
     ['inline (pow #:inline 1
                   x (ui64 5))]
     ['specialize (pow #:specialize '(2)
                       x (ui64 5))])))

(parameterize ([compile-options (list 'pretty 'dump)])
  (compile-sham-module!
   mod1
   #:opt-level 3 #:size-level 3))

(sham-app pow 2 10)
(sham-app pow5 3)
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
