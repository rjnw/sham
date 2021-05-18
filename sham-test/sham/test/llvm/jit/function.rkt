#lang racket

(require sham/llvm/ir
         sham/llvm/ir/simple
         sham/llvm/jit/mc
         sham/md)

(require "../ir/types.rkt"
         "../ir/function.rkt")

(module+ test
  (require rackunit
           ffi/unsafe)
  (define t-module
    (def-module 'function-jit-test-module
      size-array-t identity-f new-alloca-array-f pow-f))
  (define t-env (build-llvm-env t-module))
  ;; (dump-llvm-ir t-env)
  ;; (write-llvm-ir t-env "/tmp/function-test.ll")
  ;; (verify-llvm-module t-env)
  (define tc-env (initialize-mcjit t-env))
  (printf "initialized mc\n")
  (define pow-uintptr (mcjit-function-address tc-env 'pow))
  (define pow-func (cast pow-uintptr _uintptr (_fun _uint64 _uint64 -> _uint64)))
  (test-eq? "llvm-function:pow" (pow-func 2 10) 1024)

  (define new-array-uintptr (mcjit-function-address tc-env 'new-alloca-array))
  (define-cstruct _size-array ([size _uint64] [arr _pointer]))
  (define new-array-func (cast new-array-uintptr _uintptr (_fun _uint64 _pointer -> _size-array)))
  (define a (new-array-func 42 #f))
  (test-eq? "llvm-function:size-array" (size-array-size a) 42)
  )
