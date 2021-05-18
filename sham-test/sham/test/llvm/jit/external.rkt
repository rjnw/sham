#lang racket

(require sham/md
         sham/llvm/ir
         sham/llvm/ir/md
         sham/llvm/ir/simple
         sham/llvm/jit/mc)

(define call-rkt-f
  (def-function 'call-ex (type-function i64 #f i64)
    (def-block 'entry
      (inst-op 'ret 'ex-rkt #f (0))
      (inst-ret 'ret))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define rkt-f add1)
  (define ex-rkt
    (external-mapping
     'ex-rkt
     (cast
      (function-ptr rkt-f (_fun _uint64 -> _uint64))
      _pointer
      _uintptr)))
  (define t-module
    (def-module
      #:md (set-module-md-jit-external-mappings!
            (empty-module-md)
            (list
             ex-rkt))
      'external-jit-test-module
      (def-external 'ex-rkt (type-function i64 #f i64))
      call-rkt-f))
  (define t-env (build-llvm-env t-module))
  (print-llvm-ir t-env)
  (test-true "verify-external" (verify-llvm-module t-env))
  (define tc-env (initialize-mcjit t-env))

  (define call-uintptr (mcjit-function-address tc-env 'call-ex))
  (define call-func (cast call-uintptr _uintptr (_fun _uint64 -> _uint64)))
  (test-eq? "llvm-external:call" (call-func 41) 42))
