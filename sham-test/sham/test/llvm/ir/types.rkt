#lang racket

(require sham/md
         sham/llvm/ir)

(provide (all-defined-out))

(define v4-int-t (def-type 'v4i64 (type-vector i64 4)))
(define v4-int-ref (type-ref 'v4i64))

(define size-array-t (def-type 'size-array (type-struct i32 i32*)))
(define size-array-ref (type-ref 'size-array))
(define linked-list-t (def-type 'node (type-struct void* (type-pointer (type-ref 'node)))))
(define ll-node-ref (type-ref 'node))

(module+ test
  (require rackunit)
  (define all-types (def-type 'all-types
                      (type-struct size-array-ref
                                   ll-node-ref
                                   (type-vector i32 4)
                                   (type-pointer (type-function i32 i32 #f void)))))
  (define at (type-ref 'all-types))

  (define tfun
    (def-function 'testf (type-function at #f at)
      (def-block 'entry
        (op-int->ptr 'null-i32* ((val-ui 0 i32) i32*))
        (op-int->ptr 'null-void* ((val-ui 0 i32) void*))
        (op-int->ptr 'null-fptr ((val-ui 0 i32) (type-pointer (type-function i32 i32 #f void))))
        (inst-ret (val-named-struct
                   (val-named-struct (val-ui 42 i32) 'null-i32* size-array-ref)
                   (val-named-struct 'null-void* 'null-void* ll-node-ref)
                   (val-vector (val-ui 21 i32) (val-ui 42 i32) (val-ui 84 i32) (val-ui 168 i32))
                   'null-fptr
                   at)))))
  (define t-module
    (def-module 'types-test-module size-array-t linked-list-t all-types tfun))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (test-true "llvm-types" (verify-llvm-module l-env)))
