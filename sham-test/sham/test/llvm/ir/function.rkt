#lang racket
(require sham/llvm/ir
         sham/llvm/ir/simple)

(require "types.rkt")

(provide (all-defined-out))

(define identity-f
  (def-function #f
    'identity `(a) (list i32) i32
    (list (ast-block 'entry
                     '()
                     (ast-ret 'a)))))

(define new-array-f
  (def-function #f
    'new-array `(s arr) (list i32 i32*) size-array-ref
    (list (ast-block 'entry '()
                     (ast-ret (const-named-struct `(s arr) size-array-ref))))))

(define pow-f
  (def-function #f
    'pow `(x n) (list i64 i64) i64
    (list (ast-block 'entry
                     (list (icmp-ule 'check (list 'n (const-ui 0 i64))))
                     (ast-br 'check 'thn 'els))
          (ast-block 'thn
                     '()
                     (ast-ret (const-ui 1 i64)))
          (ast-block 'els
                     (list
                      (sub-nuw 'subn (list 'n (const-ui 1 i64)))
                      (ast-op 'rec 'pow #f (list 'x 'subn))
                      (mul 'result (list 'x 'rec)))
                     (ast-ret 'result)))))

(module+ test
  (require rackunit)
  (define t-module
    (def-module #f 'function-test-module
      (list size-array-t identity-f new-array-f pow-f)))
  (define l-module (build-llvm-module t-module))
  (dump-llvm-module l-module)
  (test-true "llvm-functions" (verify-llvm-module l-module)))
