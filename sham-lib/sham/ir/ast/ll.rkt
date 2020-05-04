#lang racket

(require sham/ir/ast/core
         sham/ir/ast/simple
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple)
         (rename-in sham/llvm/ir/op
                    [basic-ops llvm-basic-ops]))

(require (for-syntax syntax/parse racket/syntax)
         syntax/parse/define)

(provide (except-out (prefix-out ll- (all-defined-out))
                     define-llvm-alias
                     definer-for-llvm-ops
                     defines-for-llvm-ops))

(define-simple-macro (define-llvm-alias names:id ...)
  #:with (llvm-names ...) (map (λ (n) (format-id n "llvm-~a" n))
                               (syntax->list #`(names ...)))
  (begin (define names llvm-names) ...))

(define-llvm-alias
  def-module def-function def-type def-global def-global-string def-external def-intrinsic
  ast-block ast-op ast-retv ast-ret ast-br ast-bru ast-switch
  val-ref val-param val-fl val-si val-ui val-string val-llvm val-basic-struct val-named-struct val-array val-vector
  type-ref type-struct type-function type-pointer type-array type-vector)

(define d-global def-global)
(define d-global-string def-global-string)
(define d-external def-external)
(define d-intrinsic def-intrinsic)

(define-syntax (definer-for-llvm-ops stx)
  (syntax-parse stx
    [(_ op-names:id)
     (define names (map (λ (n) (datum->syntax stx n)) (syntax-local-value #'op-names)))
     #`(defines-for-llvm-ops #,@names)]))

(define-syntax (defines-for-llvm-ops stx)
  (syntax-parse stx
    [(_ op-name:id ...)
     #:with (def-name ...) (map (λ (n) (format-id n "e-~a" n)) (syntax->list #`(op-name ...)))
     #`(begin
         (define (def-name #:flags (flags #f) args)
           (e-op (quote op-name) flags args))
         ...)]))

(definer-for-llvm-ops llvm-basic-ops)
