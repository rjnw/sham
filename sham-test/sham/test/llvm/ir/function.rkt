#lang racket

(require sham/llvm/ir
         sham/md)

(require "types.rkt")

(provide (all-defined-out))

(define identity-f
  (def-function
    'identity (type-function i32 #f i32)
    (def-block 'entry
               (inst-ret 0))))

(define new-array-f
  (def-function
    'new-array (type-function i32 i32* #f size-array-ref)
    (def-block 'entry
               (inst-ret (val-named-struct 0 1 size-array-ref)))))

(define pow-f
  (def-function
    'pow (type-function i64 i64 #f i64)
    (def-block 'entry
      (op-icmp-ule 'check ('1 (val-ui 0 i64)))
      (inst-br 'check 'thn 'els))
    (def-block 'thn
      (inst-ret (val-ui 1 i64)))
    (def-block 'els
      (op-sub-nuw 'subn (1 (val-ui 1 i64)))
      (inst-op 'rec 'pow #f (0 'subn))
      (op-mul 'result (0 'rec))
      (inst-ret 'result))))

(define new-alloca-array-f
  (def-function 'new-alloca-array (type-function i32 i32* #f size-array-ref)
    (def-block 'entry
      (op-alloca 'ret-ptr (size-array-ref))
      (op-gep 'ret-size ('ret-ptr (val-ui 0 i32) (val-ui 0 i32)))
      (op-store! #f ('0 'ret-size))
      (op-gep 'ret-arr ('ret-ptr (val-ui 0 i32) (val-ui 1 i32)))
      (op-store! #f ('1 'ret-arr))
      (op-load 'ret ('ret-ptr))
      (inst-ret 'ret))))

(module+ test
  (require rackunit)
  (define t-module
    (def-module 'function-test-module
      size-array-t identity-f new-array-f new-alloca-array-f pow-f))
  (define l-env (build-llvm-env t-module))
  (print-llvm-ir l-env)
  (test-true "llvm-functions" (verify-llvm-module l-env)))
