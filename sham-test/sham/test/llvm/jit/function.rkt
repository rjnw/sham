#lang racket

(require sham/llvm/ir
         sham/llvm/ir/simple
         sham/llvm/jit/mc
         sham/md)

(require sham/test/llvm/ir/types
         sham/test/llvm/ir/function)

(define new-array-f
  (def-function (empty-function-md)
    'new-array (type-function (list i32 i32*) #f size-array-ref)
    (list (ast-block 'entry
                     (list
                      (op-alloca 'ret-ptr (list size-array-ref))
                      (op-gep 'ret-size (list 'ret-ptr (val-ui 0 i32) (val-ui 0 i32)))
                      (op-store! #f (list '0 'ret-size))
                      (op-gep 'ret-arr (list 'ret-ptr (val-ui 0 i32) (val-ui 1 i32)))
                      (op-store! #f (list '1 'ret-arr))
                      (op-load 'ret (list 'ret-ptr)))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define t-module
    (def-module (empty-module-md) 'function-jit-test-module
      (list size-array-t identity-f new-array-f pow-f)))
  (define t-env (build-llvm-env t-module))
  (dump-llvm-ir t-env)
  ;; (write-llvm-ir t-env "/tmp/function-test.ll")
  (verify-llvm-module t-env)
  (define tc-env (initialize-mcjit t-env))

  (define pow-uintptr (mcjit-function-address tc-env 'pow))
  (define pow-func (cast pow-uintptr _uintptr (_fun _uint64 _uint64 -> _uint64)))
  (test-eq? "llvm-function:pow" (pow-func 2 10) 1024)

  (define new-array-uintptr (mcjit-function-address tc-env 'new-array))
  (define-cstruct _size-array ([size _uint64] [arr _pointer]))
  (define new-array-func (cast new-array-uintptr _uintptr (_fun _uint64 _pointer -> _size-array)))
  (define a (new-array-func 42 #f))
  (test-eq? "llvm-function:size-array" (size-array-size a) 42))
