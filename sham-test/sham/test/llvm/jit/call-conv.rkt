#lang racket

(require sham/md
         sham/llvm/ir
         sham/llvm/ir/callconv
         sham/llvm/jit/mc)

(define a-f
  (def-function (fastcc! (empty-function-md))
    'a (type-function (list i64 i64 i64) #f i64)
    (list (ast-block 'entry
                     (list (op-icmp-eq 'cond (list 0 (val-ui 0 i64))))
                     (ast-br 'cond 'then 'else))
          (ast-block 'then
                     `()
                     (ast-ret 2))
          (ast-block 'else
                     (list
                      (op-sub 'subi (list 0 (val-ui 1 i64)))
                      (op-add-nuw 'resv (list 2 1))
                      (fastcc! (ast-op 'ret 'b #f (list 'subi 1 'resv))))
                     (ast-ret 'ret)))))

(define b-f
  (def-function (fastcc! (empty-function-md))
    'b (type-function (list i64 i64 i64) #f i64)
    (list (ast-block 'entry
                     (list (op-icmp-eq 'cond (list 0 (val-ui 0 i64))))
                     (ast-br 'cond 'then 'else))
          (ast-block 'then
                     `()
                     (ast-ret 2))
          (ast-block 'else
                     (list
                      (op-sub 'subi (list 0 (val-ui 1 i64)))
                      (fastcc! (ast-op 'ret 'a #f (list 'subi 1 2))))
                     (ast-ret 'ret)))))
(define wrap-f
  (def-function (empty-function-md)
    'wrap (type-function (list i64) #f i64)
    (list (ast-block 'entry
                     (list
                      (fastcc! (ast-op 'ret 'b #f (list 0 0 (val-ui 1 i64)))))
                     (ast-ret 'ret)))))

(module+ test
  (require rackunit ffi/unsafe)
  (define t-module
    (def-module (empty-module-md) 'call-conv-test-module
      (list a-f b-f wrap-f)))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (test-true "verify-call-conv" (verify-llvm-module l-env))

  (define mc-env (initialize-mcjit l-env))
  (define wrap-uintptr (mcjit-function-address mc-env 'wrap))
  (define wrap-func (cast wrap-uintptr _uintptr (_fun _uint64 -> _uint64)))
  (test-eq? "call-conv-wrap" (wrap-func 100000) 5000000001))
