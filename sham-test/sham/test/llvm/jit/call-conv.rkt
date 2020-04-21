#lang racket

(require sham/llvm/ir
         sham/llvm/ir/md
         sham/llvm/ir/simple
         sham/llvm/jit/mc
         sham/llvm/ir/callconv/fastcc)

(define a-f
  (def-function (fastcc! (empty-function-info))
    'a `(i v res) (list i64 i64 i64) i64
    (list (ast-block 'entry
                     (list (icmp-eq 'cond (list 'i (const-ui 0 i64))))
                     (ast-br 'cond 'then 'else))
          (ast-block 'then
                     `()
                     (ast-ret 'res))
          (ast-block 'else
                     (list
                      (sub 'subi (list 'i (const-ui 1 i64)))
                      (add-nuw 'resv (list 'res 'v))
                      (fastcc! (ast-op 'ret 'b #f (list 'subi 'v 'resv))))
                     (ast-ret 'ret)))))

(define b-f
  (def-function (fastcc! (empty-function-info))
    'b `(i v res) (list i64 i64 i64) i64
    (list (ast-block 'entry
                     (list (icmp-eq 'cond (list 'i (const-ui 0 i64))))
                     (ast-br 'cond 'then 'else))
          (ast-block 'then
                     `()
                     (ast-ret 'res))
          (ast-block 'else
                     (list
                      (sub 'subi (list 'i (const-ui 1 i64)))
                      (fastcc! (ast-op 'ret 'a #f (list 'subi 'v 'res))))
                     (ast-ret 'ret)))))
(define wrap-f
  (def-function (empty-function-info)
    'wrap `(inp) (list i64) i64
    (list (ast-block 'entry
                     (list
                      (fastcc! (ast-op 'ret 'b #f (list 'inp 'inp (const-ui 1 i64)))))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit ffi/unsafe)
  (define t-module
    (def-module (empty-module-info) 'call-conv-test-module
      (list a-f b-f wrap-f)))
  (define l-module (build-llvm-module t-module))
  (dump-llvm-module l-module)
  (test-true "verify-call-conv" (verify-llvm-module l-module))

  (define mc-module (llvm-initialize-mcjit l-module))
  (define wrap-uintptr (mcjit-function-address mc-module 'wrap))
  (define wrap-func (cast wrap-uintptr _uintptr (_fun _uint64 -> _uint64)))
  (test-eq? "call-conv-wrap" (wrap-func 100000) 5000000001))
