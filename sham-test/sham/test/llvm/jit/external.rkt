#lang racket

(require sham/llvm/ir
         sham/llvm/ir/md
         sham/llvm/ir/simple
         sham/llvm/jit/mc)

(define call-rkt-f
  (def-function (empty-function-info)
    'call-ex `(a) (list i64) i64
    (list (ast-block 'entry
                     (list
                      (ast-op 'ret 'ex-rkt #f (list 'a)))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define rkt-f add1)
  (define t-module
    (def-module (module-info-external-mappings
                 (empty-module-info) (list (external-mapping 'ex-rkt
                                                             (cast
                                                              (function-ptr rkt-f (_fun _uint64 -> _uint64))
                                                              _pointer
                                                              _uintptr))))
      'external-jit-test-module
      (list (def-external #f 'ex-rkt (type-function (list i64) i64)) call-rkt-f)))
  (define t-env (build-llvm-module t-module))
  (dump-llvm-module t-env)
  (test-true "verify-external" (verify-llvm-module t-env))
  (define tc-module (llvm-initialize-mcjit t-env))

  (define call-uintptr (mcjit-function-address tc-module 'call-ex))
  (define call-func (cast call-uintptr _uintptr (_fun _uint64 -> _uint64)))
  (test-eq? "llvm-external:call" (call-func 41) 42))
