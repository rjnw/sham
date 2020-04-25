#lang racket
(require sham/llvm/ir
         sham/llvm/ir/simple)

(require "types.rkt")

(provide (all-defined-out))

(define identity-f
  (def-function #f
    'identity (type-function (list i32) #f i32)
    (list (ast-block 'entry
                     '()
                     (ast-ret 0)))))

(define new-array-f
  (def-function #f
    'new-array (type-function (list i32 i32*) #f size-array-ref)
    (list (ast-block 'entry '()
                     (ast-ret (val-named-struct `(0 1) size-array-ref))))))

(define pow-f
  (def-function #f
    'pow (type-function (list i64 i64) #f i64)
    (list (ast-block 'entry
                     (list (icmp-ule 'check (list '1 (val-ui 0 i64))))
                     (ast-br 'check 'thn 'els))
          (ast-block 'thn
                     '()
                     (ast-ret (val-ui 1 i64)))
          (ast-block 'els
                     (list
                      (sub-nuw 'subn (list 1 (val-ui 1 i64)))
                      (ast-op 'rec 'pow #f (list 0 'subn))
                      (mul 'result (list 0 'rec)))
                     (ast-ret 'result)))))

(module+ test
  (require rackunit)
  (define t-module
    (def-module #f 'function-test-module
      (list size-array-t identity-f new-array-f pow-f)))
  (define l-module (build-llvm-module t-module))
  (dump-llvm-module l-module)
  (test-true "llvm-functions" (verify-llvm-module l-module)))
