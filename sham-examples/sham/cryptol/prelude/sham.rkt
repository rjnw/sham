#lang racket

(require sham/ir
         sham/jit
         sham/md
         (prefix-in ll- sham/llvm/ir))

(define (store-basic-val! val result)
  (stmt-expr (op-store! val result)))

(define (basic-function-type args result)
  (ll-make-type-function (append args (list (ll-type-pointer result))) #f i64))

(define (print-yay str)
  (expr-app (rator-external #f "printf" i64 #t) str))
