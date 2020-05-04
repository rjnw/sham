#lang racket

(require sham/md
         sham/llvm/ir
         sham/llvm/ir/simple
         sham/llvm/jit/mc)

(define pow-f
  (def-function (empty-function-md)
    'pow-f (type-function (list f64 f64) #f f64)
    (list (ast-block 'entry
                     (list
                      (ast-op 'ret 'pow #f `(0 1)))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define t-module
    (def-module (empty-module-md)
      'intrinsic-test-module
      (list (def-intrinsic #f 'pow "llvm.pow.f64" (type-function (list f64 f64) #f f64))
            pow-f)))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (verify-llvm-module l-env)
  (define tc-env (initialize-mcjit l-env))
  (define pow-uintptr (mcjit-function-address tc-env 'pow-f))
  (define pow-func (cast pow-uintptr _uintptr (_fun _double _double -> _double)))
  (test-= "llvm-function:pow" (pow-func 2.0 10.0) 1024.0 0.001))
