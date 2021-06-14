#lang racket

(require (for-syntax syntax/parse)
         (for-template racket/base))
(require sham/ir/ast
         sham/llvm/ir/ast
         (prefix-in ll- sham/llvm/ir/simple)
         sham/sam/alias)

(provide (all-defined-out))

(define-aliases sham
  [def -> def]
  [rator -> rator]
  [stmt -> stmt]
  [expr -> expr])

(define-syntax (function stx)
  (syntax-parse stx
    [(_ (name:id (arg-ids:id (~optional (~datum :)) arg-types) ... (~datum :) ret-type) body ...)
     #:with (arg-cnts ...) (build-list (length (syntax-e #`(arg-ids ...))) (λ (i) (datum->syntax stx i)))
     #`(def-function 'name (ll-type-function arg-types ... #f ret-type)
         (let ([arg-ids (ll-val-param arg-cnts)] ...)
           (stmt-block body ...)))]))

(define (expr-app rator . rands)
  (match rator
    [(? rator?) (make-expr-op rator rands)]
    [(expr-ref v) (make-expr-op (rator-reference v) rands)]
    [(? symbol?) (make-expr-op (rator-reference rator) rands)]
    [(? string?)
     (if (llvm:type? (car rands))
         (make-expr-op (rator-intrinsic rator (car rands)) (cdr rands))
         (error 'sham:ir "expected type for intrinsic as first argument, ~a/~a" rator (car rands)))]
    [(? procedure?) (apply rator rands)]
    [else (error 'sham:ir "expected rator for app^ given: ~a" rator)]))
