#lang racket

(require sham/ir/ast/core
         sham/ir/ast/ll
         sham/llvm/ir/ast)

(require (for-syntax racket/syntax syntax/parse))

(define-syntax (definer-for-ops stx)
  (syntax-parse stx
    [(_ op-names:id)
     #:with (name ...) (map (λ (n) (datum->syntax stx n)) (syntax-local-value #'op-names))
     #:with (name^ ...) (map (λ (n) (format-id n "~a^" n)) (syntax->list #`(name ...)))
     #:with (llvm-name ...) (map (λ (n) (format-id n "e-llvm-~a" n)) (syntax->list #`(name ...)))
     #`(begin
         (define (name #:flags (flags #f) args) (llvm-name #:flags flags args)) ...
         (define (name^ #:flags (flags #f) . args) (llvm-name #:flags flags args)) ...)]))
