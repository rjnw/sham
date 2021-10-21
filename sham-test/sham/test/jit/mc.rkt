#lang racket

(require sham/md
         sham/ir
         sham/llvm/ir/md
         sham/jit)
(require (prefix-in raw- sham/test/ir/raw)
         (prefix-in syn- sham/test/ir/syntax))

(module+ test
  (require rackunit)
  (require sham/ir/dump
           sham/ir/verify
           sham/ir/optimize)
  (define (test-pow-identity info-str pow-f identity-f #:opt-level (opt-level #f))
    (define t-module
      (def-module (format "sham-jit-mc-test-module:~a" info-str)
        identity-f pow-f))
    (define s-env (build-sham-env t-module))
    (sham-print-llvm-ir s-env)
    (when opt-level
      (sham-env-optimize-llvm! s-env #:opt-level opt-level)
      (test-true (format "sham-mcjit:verify:~a" info-str) (sham-verify-llvm-ir s-env))
      (printf "optmized with level-~a:\n" opt-level)
      (sham-print-llvm-ir s-env))
    (define mc-env (initialize-jit s-env))
    (define pow-func (jit-lookup-function mc-env 'pow))
    (define identity-func (jit-lookup-function mc-env 'identity))
    (test-eq? (format "sham-mcjit:pow:~a" info-str) (pow-func 2 10) 1024)
    (test-eq? (format "sham-mcjit:identity:~a" info-str) (identity-func 42) 42))
  (for ([opt-level (list #f 1 2 3)])
    (test-pow-identity (format "raw-o~a" opt-level) raw-pow-f raw-identity-f #:opt-level opt-level)
    (test-pow-identity (format "syntax-o~a" opt-level) syn-pow-f syn-identity-f #:opt-level opt-level)))
