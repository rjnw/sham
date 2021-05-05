#lang racket

(require sham/ir/ast
         sham/llvm/ir/ast
         sham/sam/alias)

(provide (all-defined-out))

(define-aliases sham
  [def -> def]
  [rator -> rator]
  [stmt -> stmt]
  [expr -> expr])

(define (expr-app rator flags rands)
  (match rator
    [(? rator?) (make-expr-op rator flags rands)]
    [(expr-ref v) (make-expr-op (rator-ref v) flags rands)]
    [(? symbol?) (make-expr-op (rator-ref rator) flags rands)]
    [(? string?)
     (if (llvm:type? (car rands))
         (make-expr-op (rator-intrinsic rator (car rands)) flags (cdr rands))
         (error 'sham:ir "expected type for intrinsic as first argument, ~a/~a" rator (car rands)))]
    [(? procedure?) (apply rator rands)]
    [else (error 'sham:ir "expected rator for app^ given: ~a" rator)]))
