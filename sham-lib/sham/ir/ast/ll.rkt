#lang racket

(require sham/ast/core
         sham/llvm/ir/ast
         sham/llvm/ir/simple)

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
     #:with def-name (format-id #'op-name "e-llvm-~a" #'op-name)
     #`(begin
         (define (def-name #:flags (flags #f) args)
           (e-op (quote op-name) flags args))
         ...)]))

(definer-for-llvm-ops basic-ops)
