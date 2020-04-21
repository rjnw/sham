#lang racket

(require sham/llvm/ir
         sham/llvm/ir/simple
         sham/llvm/jit/mc)

(require sham/test/llvm/ir/types
         sham/test/llvm/ir/function)

(define new-array-f
  (def-function #f
    'new-array `(s arr) (list i32 i32*) size-array-ref
    (list (ast-block 'entry
                     (list
                      (alloca 'ret-ptr (list size-array-ref))
                      (gep 'ret-size (list 'ret-ptr (const-ui 0 i32) (const-ui 0 i32)))
                      (store! #f (list 's 'ret-size))
                      (gep 'ret-arr (list 'ret-ptr (const-ui 0 i32) (const-ui 1 i32)))
                      (store! #f (list 'arr 'ret-arr))
                      (load 'ret (list 'ret-ptr)))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit
           ffi/unsafe)
  (define t-module
    (def-module (empty-module-info) 'function-jit-test-module
      (list size-array-t identity-f new-array-f pow-f)))
  (define t-env (build-llvm-module t-module))
  (dump-llvm-module t-env)
  ;; (write-llvm-module t-env "/tmp/function-test.ll")
  (verify-llvm-module t-env)
  (define tc-module (llvm-initialize-mcjit t-env))

  (define pow-uintptr (mcjit-function-address tc-module 'pow))
  (define pow-func (cast pow-uintptr _uintptr (_fun _uint64 _uint64 -> _uint64)))
  (test-eq? "llvm-function:pow" (pow-func 2 10) 1024)

  (define new-array-uintptr (mcjit-function-address tc-module 'new-array))
  (define-cstruct _size-array ([size _uint64] [arr _pointer]))
  (define new-array-func (cast new-array-uintptr _uintptr (_fun _uint64 _pointer -> _size-array)))
  (define a (new-array-func 42 #f))
  (test-eq? "llvm-function:size-array" (size-array-size a) 42))
