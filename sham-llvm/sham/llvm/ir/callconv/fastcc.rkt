#lang racket

(require sham/md
         sham/private/env
         sham/llvm/ir/ast
         sham/llvm/ir/env)

(provide fastcc!)

(define fastcc-calling-convention 'Fast)

(define (fastcc! v)
  (match v
    [(? function-md?)
     (set-function-md-llvm-calling-convention! v fastcc-calling-convention)]
    [(? instruction-md?)
     (set-instruction-md-llvm-calling-convention! v fastcc-calling-convention)]
    [(? (or/c assoc-env? hash?)) (general-env-fastcc v)]
    [(? (or/c llvm:def:function? llvm:ast:instruction:op?)) (fastcc! (llvm-metadata v)) v]
    [else (general-env-fastcc)]))

(define (general-env-fastcc (env (empty-assoc-env)))
  (general-set-key env instruction-md-llvm-calling-convention fastcc-calling-convention))
