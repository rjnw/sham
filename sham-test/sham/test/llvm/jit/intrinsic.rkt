#lang racket

(require sham/llvm/ir
         sham/llvm/ir/simple
         sham/llvm/jit/mc)

(define pow-f
  (def-function (empty-function-info)
    'pow-f `(val power) (list f64 f64) f64
    (list (ast-block 'entry
                     (list
                      (ast-op 'ret 'pow #f (list 'val 'power)))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define t-module
    (def-module (empty-module-info)
      'intrinsic-test-module
      (list (def-intrinsic #f 'pow "llvm.pow.f64" (type-function (list f64 f64) f64))
            pow-f)))
  (define l-module (build-llvm-module t-module))
  (dump-llvm-module l-module)
  (verify-llvm-module l-module)
  (define tc-module (llvm-initialize-mcjit l-module))
  (define pow-uintptr (mcjit-function-address tc-module 'pow-f))
  (define pow-func (cast pow-uintptr _uintptr (_fun _double _double -> _double)))
  (test-= "llvm-function:pow" (pow-func 2.0 10.0) 1024.0 0.001))
