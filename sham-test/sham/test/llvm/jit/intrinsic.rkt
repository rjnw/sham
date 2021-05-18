#lang racket

(require sham/md
         sham/llvm/ir
         sham/llvm/ir/simple
         sham/llvm/jit/mc)

(define pow-f
  (def-function 'pow-f
    (type-function f64 f64 #f f64)
    (def-block 'entry
      (inst-op 'ret 'pow #f (0 1))
      (inst-ret 'ret))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define t-module
    (def-module 'intrinsic-test-module
      (def-intrinsic 'pow "llvm.pow.f64" (type-function f64 f64 #f f64))
      pow-f))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (test-true "verify" (verify-llvm-module l-env))
  (define tc-env (initialize-mcjit l-env))
  (define pow-uintptr (mcjit-function-address tc-env 'pow-f))
  (define pow-func (cast pow-uintptr _uintptr (_fun _double _double -> _double)))
  (test-= "llvm-function:pow" (pow-func 2.0 10.0) 1024.0 0.001))
