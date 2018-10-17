#lang racket
(require "../main.rkt"
         "../private/ast-utils.rkt")
(require "../private/llvm/ffi/all.rkt")
(define-sham-function
  (pow (x : i32) (n : i32)) : i32
  (if^ (icmp-ule n (ui32 0))
       (return (ui32 1))
       (return (mul x (pow x (sub-nuw n (ui32 1)))))))

(define-sham-function
  (pow2 (x : i32) (n : i32)) : i32
  (if^ (icmp-ule n (ui32 0))
       (return (ui32 1))
       (return (mul x (pow2 x (sub-nuw n (ui32 1)))))))

(module+ test
  (define-module test-module
    (empty-mod-env-info)
    (list pow))
  (define test-mod-env (compile-module test-module))


  (define-module test-module2
    (empty-mod-env-info)
    (list pow2))
  (define test-mod-env2 (compile-module test-module2))
  (define llvm-mod1 (env-get-module test-mod-env))
  (define mod1c (LLVMCloneModule llvm-mod1))
  (define llvm-mod2 (env-get-module test-mod-env2))
  (define mod-combine (LLVMLinkModules2 llvm-mod2 mod1c)) ;; destroys mod1c
  (define orc2 (create-orc-instance))
  (orc-get-function-address orc2 'pow2)
  (define module-handle (orc-add-lazy-llvm-module! orc2 llvm-mod2))

  (orc-get-function-address orc2 'pow)

  (jit-verify-module test-mod-env)
  ;; (optimize-module test-mod-env #:opt-level 3)
  (jit-dump-module test-mod-env)
  (module-initialize-orc! test-mod-env)
  ;; (initialize-jit! test-mod-env)
  (define p (jit-get-function 'pow test-mod-env))
  (p 3 3)




  )
