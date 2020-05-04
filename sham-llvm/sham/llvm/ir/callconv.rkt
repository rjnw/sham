#lang racket

(require sham/md
         sham/llvm/ir/ast
         sham/private/env
         sham/private/maybe)

(provide fastcc!
         callconv!
         fastcc-md)

(define fastcc-calling-convention 'Fast)
(define fastcc-md fastcc-calling-convention)

(define (callconv! v conv)
  (case (string-downcase (normalize-id conv))
    [("fastcc" "fast") (fastcc! v)]))

(define (fastcc! v)
  (set-callconv! v fastcc-calling-convention))

(define (set-callconv! v conv-key)
  (match v
    [(? function-md?)
     (set-function-md-llvm-calling-convention! v conv-key)]
    [(? instruction-md?)
     (set-instruction-md-llvm-calling-convention! v conv-key)]
    [(? llvm:def:function?)
     (llvm-metadata! (set-callconv! (from-maybe (llvm-metadata v) empty-function-md) conv-key))]
    [(? llvm:ast:instruction:op?)
     (llvm-metadata! v (set-callconv! (from-maybe (llvm-metadata v) empty-instruction-md) conv-key))]
    [(? (or/c assoc-env? hash?))
     (general-env-callconv! v conv-key)]
    [else (general-env-callconv! conv-key)]))

(define (general-env-callconv! conv-key (env (empty-assoc-env)))
  (general-set-key env instruction-md-llvm-calling-convention conv-key))
