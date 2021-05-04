#lang racket

(require sham/ir/ast/core
         sham/llvm/ir/ast
         sham/sam/alias)

(provide (all-defined-out))

(define-aliases sham
  #:sep -
  [def -> def]
  [rator -> rator]
  [stmt -> stmt]
  [expr -> expr])

(define (expr-app rator flags rands)
  (match rator
    [(? sham:rator?) (make-sham:expr:op rator flags rands)]
    [(sham:expr:ref v) (make-sham:expr:op (rator-ref v) flags rands)]
    [(? symbol?) (make-sham:expr:op (rator-ref rator) flags rands)]
    [(? string?)
     (if (llvm:type? (car rands))
         (make-sham:expr:op (rator-intrinsic rator (car rands)) flags (cdr rands))
         (error 'sham:ir "expected type for intrinsic as first argument, ~a/~a" rator (car rands)))]
    [(? procedure?) (apply rator rands)]
    [else (error 'sham:ir "expected rator for app^ given: ~a" rator)]))
