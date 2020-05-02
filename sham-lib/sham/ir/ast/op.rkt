#lang racket

(require sham/ir/ast/core
         sham/ir/ast/ll
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple))

(require (for-syntax racket/syntax syntax/parse))

(provide (except-out (all-defined-out)
                     definer-for-llvm-ops
                     app app^))

(define-syntax (definer-for-llvm-ops stx)
  (syntax-parse stx
    [(_ op-names:id)
     #:with (name ...) (map (λ (n) (datum->syntax stx n)) (syntax-local-value #'op-names))
     #:with (name^ ...) (map (λ (n) (format-id n "~a^" n)) (syntax->list #`(name ...)))
     #:with (llvm-name ...) (map (λ (n) (format-id n "ll-e-~a" n)) (syntax->list #`(name ...)))
     #`(begin
         (define (name #:flags (flags #f) args) (llvm-name #:flags flags args)) ...
         (define (name^ #:flags (flags #f) . args) (llvm-name #:flags flags args)) ...)]))

(definer-for-llvm-ops llvm-basic-ops)
