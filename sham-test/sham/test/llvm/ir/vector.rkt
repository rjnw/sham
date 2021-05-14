#lang racket
(require sham/llvm/ir
         sham/llvm/ir/simple)

(require "types.rkt")

(provide (all-defined-out))

(define v4-add-f
  (def-function
    'v4-add (make-llvm:type:function (build-list 8 (const i64)) #f i64)
    (def-block 'entry

      (op-add-nuw 'vadd (make-llvm:value:vector (build-list 4 (const (val-ui 100 i64))))
                  (make-llvm:value:vector (build-list 4 (const (val-ui 100 i64)))))
      (op-extract-element 'v0 'vadd (val-ui 0 i64))
      (op-extract-element 'v1 'vadd (val-ui 1 i64))
      (op-extract-element 'v2 'vadd (val-ui 2 i64))
      (op-extract-element 'v3 'vadd (val-ui 3 i64))
      (op-extract-element 'test (make-llvm:value:vector (build-list 4 (const (val-ui 100 i64))))
                          (val-ui 0 i64))
      (op-add-nuw 'result 'v0 'v1 'v2 'v3)
      (inst-ret 'result))))

(module+ test
  (require rackunit)
  (define t-module (def-module 'vector-test-module v4-add-f))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (test-true "llvm-vectors" (verify-llvm-module l-env)))
