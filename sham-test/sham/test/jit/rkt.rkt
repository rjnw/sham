#lang racket

(require sham/md
         sham/ir
         sham/llvm/ir/md
         sham/llvm/ir/env
         sham/jit
         sham/rkt
         sham/rkt/conv
         ffi/unsafe
         (prefix-in ll- sham/llvm/ir))

(define rkt-printf (λ (arg) (printf "rkt-print:~a\n" arg) 0))
(define rkt-print (ll-def-external 'rkt-print (ll-type-function i64 #f i64)))

(define print-foo
  (function (print-foo : i64) (stmt-return (expr-app 'rkt-print (ui64 42)))))

(define sham-md
  #;(set-module-md-jit-external-mappings!
     (empty-module-md)
     (list (external-mapping 'rkt-print (func-uintptr rkt-printf (_fun _uint64 -> _uint64)))))
  (sham-module-metadata #:externals (list (external-mapping 'rkt-print (func-uintptr rkt-printf (_fun _uint64 -> _uint64))))))

(define sham-module
  (def-module
    #:md sham-md
    "test-module"
    rkt-print
    print-foo))

(define built-sham-env (build-sham-env sham-module))
(sham-print-llvm-ir built-sham-env)


(sham-env-optimize-llvm! built-sham-env #:opt-level 2)
(sham-verify-llvm-ir built-sham-env)
(sham-print-llvm-ir built-sham-env)

(define mc-env (initialize-jit built-sham-env))

(module+ test
  ((jit-lookup-function mc-env 'print-foo)))
