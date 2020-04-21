#lang racket

(require sham/llvm/ir/md
         sham/llvm/ir/ast
         sham/llvm/ir/env)

(provide fastcc!)

(define (fastcc! v)
  (match v
    [#f (assoc-env-fastcc)]
    [(? assoc-env?) (assoc-env-fastcc v)]
    [(? hash?) (hash-env-fastcc v)]
    [(? llvm:def:function?) (def-function-fastcc v)]
    [(? llvm:ast:instruction:op?) (op-fastcc v)]
    [else (assoc-env-fastcc)]))

(define fastcc-calling-convention 'Fast)

(define (assoc-env-fastcc (env (empty-assoc-env)))
  (assoc-env-extend env calling-convention-key fastcc-calling-convention))

(define (hash-env-fastcc (hsh (make-hash)))
  (cond [(immutable? hsh) (hash-set hsh calling-convention-key fastcc-calling-convention)]
        [else (begin (hash-set! hsh calling-convention-key fastcc-calling-convention) hsh)]))

(define (def-function-fastcc def)
  (set-llvm:def-info! def (fastcc! (llvm:def-info def)))
  def)
(define (op-fastcc op)
  (set-llvm:ast-metadata! op (fastcc! (llvm:ast-metadata op)))
  op)
