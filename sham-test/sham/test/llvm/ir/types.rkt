#lang racket

(require sham/llvm/ir
         sham/llvm/ir/simple)

(provide (all-defined-out))

(define v4-int-t (def-type #f 'v4i64 (type-vector i64 4)))
(define v4-int-ref (type-ref 'v4i64))

(define size-array-t (def-type #f 'size-array (type-struct (list i32 i32*))))
(define size-array-ref (type-ref 'size-array))
(define linked-list-t (def-type #f 'node (type-struct (list void* (type-pointer (type-ref 'node))))))
(define ll-node-ref (type-ref 'node))

(module+ test
  (require rackunit)
  (define all-types (def-type #f 'all-types
                      (type-struct (list size-array-ref
                                         ll-node-ref
                                         (type-vector i32 4)
                                         (type-pointer (type-function (list i32 i32) void))))))
  (define at (type-ref 'all-types))

  (define tfun
    (def-function #f
      'testf `(a) (list at) at
      (list (ast-block 'entry
                       (list
                        (int->ptr 'null-i32* (list (const-ui 0 i32) i32*))
                        (int->ptr 'null-void* (list (const-ui 0 i32) void*))
                        (int->ptr 'null-fptr (list (const-ui 0 i32)
                                                   (type-pointer (type-function (list i32 i32) void)))))
                       (ast-ret (const-named-struct
                                 (list
                                  (const-named-struct (list (const-ui 42 i32) 'null-i32*)
                                                      size-array-ref)
                                  (const-named-struct (list 'null-void* 'null-void*)
                                                      ll-node-ref)
                                  (const-vector (list (const-ui 21 i32)
                                                      (const-ui 42 i32)
                                                      (const-ui 84 i32)
                                                      (const-ui 168 i32)))
                                  'null-fptr)
                                 at))))))
  (define t-module
    (def-module #f 'types-test-module
      (list size-array-t linked-list-t all-types tfun)))
  (define l-module (build-llvm-module t-module))
  (dump-llvm-module l-module)
  (test-true "llvm-types" (verify-llvm-module l-module)))
