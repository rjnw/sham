#lang racket

(require sham/md
         sham/llvm/ir
         sham/llvm/ir/callconv
         sham/llvm/jit/mc)

(define a-f
  (def-function #:md (fastcc! (empty-function-md))
    'a (type-function i64 i64 i64 #f i64)
    (def-block 'entry
      (op-icmp-eq 'cond (0 (val-ui 0 i64)))
      (inst-br 'cond 'then 'else))
    (def-block 'then
      (inst-ret 2))
    (def-block 'else
      (op-sub 'subi (0 (val-ui 1 i64)))
      (op-add-nuw 'resv (2 1))
      (inst-op #:md (fastcc! (empty-instruction-md)) 'ret 'b #f ('subi 1 'resv))
      (inst-ret 'ret))))

(define b-f
  (def-function #:md (fastcc! (empty-function-md))
    'b (type-function i64 i64 i64 #f i64)
    (def-block 'entry
      (op-icmp-eq 'cond (0 (val-ui 0 i64)))
      (inst-br 'cond 'then 'else))
    (def-block 'then
      (inst-ret 2))
    (def-block 'else
      (op-sub 'subi (0 (val-ui 1 i64)))
      (inst-op #:md (fastcc! (empty-instruction-md)) 'ret 'a #f ('subi 1 2))
      (inst-ret 'ret))))
(define wrap-f
  (def-function
    'wrap (type-function i64 #f i64)
    (def-block 'entry
      (inst-op #:md (fastcc! (empty-instruction-md)) 'ret 'b #f (0 0 (val-ui 1 i64)))
      (inst-ret 'ret))))

(module+ test
  (require rackunit ffi/unsafe)
  (define t-module
    (def-module 'call-conv-test-module
      a-f b-f wrap-f))
  (define l-env (build-llvm-env t-module))
  (dump-llvm-ir l-env)
  (test-true "verify-call-conv" (verify-llvm-module l-env))

  (define mc-env (initialize-mcjit l-env))
  (define wrap-uintptr (mcjit-function-address mc-env 'wrap))
  (define wrap-func (cast wrap-uintptr _uintptr (_fun _uint64 -> _uint64)))
  (test-eq? "call-conv-wrap" (wrap-func 100000) 5000000001))
