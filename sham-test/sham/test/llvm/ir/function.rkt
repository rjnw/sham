#lang racket

(require sham/llvm/ir
         sham/md)

(require "types.rkt")

(provide (all-defined-out))

(define identity-f
  (def-function (empty-function-md)
    'identity (type-function (list i32) #f i32)
    (list (ast-block 'entry
                     '()
                     (ast-ret 0)))))

(define new-array-f
  (def-function (empty-function-md)
    'new-array (type-function (list i32 i32*) #f size-array-ref)
    (list (ast-block 'entry '()
                     (ast-ret (val-named-struct `(0 1) size-array-ref))))))

(define pow-f
  (def-function (empty-function-md)
    'pow (type-function (list i64 i64) #f i64)
    (list (ast-block 'entry
                     (list (op-icmp-ule 'check (list '1 (val-ui 0 i64))))
                     (ast-br 'check 'thn 'els))
          (ast-block 'thn
                     '()
                     (ast-ret (val-ui 1 i64)))
          (ast-block 'els
                     (list
                      (op-sub-nuw 'subn (list 1 (val-ui 1 i64)))
                      (ast-op 'rec 'pow #f (list 0 'subn))
                      (op-mul 'result (list 0 'rec)))
                     (ast-ret 'result)))))

(module+ test
  (require rackunit)
  (define t-module
    (def-module (empty-function-md) 'function-test-module
      (list size-array-t identity-f new-array-f pow-f)))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (test-true "llvm-functions" (verify-llvm-module l-env)))
