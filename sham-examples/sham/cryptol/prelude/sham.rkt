#lang racket

(require sham/ir
         sham/jit
         sham/md
         syntax/parse/define
         (prefix-in ll- sham/llvm/ir))
(provide (all-defined-out))

(define primitive-true (ui1 1))
(define primitive-false (ui1 0))
(define test-function-type (ll-type-function #f ll-void))
(define (basic-function-type . args) (ll-make-type-function args #f i64))
(define-syntax (basic-function-body stx)
  (syntax-case stx ()
    [(_ bodys ...) #`(stmt-block bodys ... (stmt-return (ui64 0)))]))

(define (store-basic-val! val ptr)
  (stmt-expr (op-store! val ptr)))

(define-syntax-parse-rule (cprintf str args ...)
  (stmt-expr (expr-app 'printf (ll-val-string str) args ...)))

(define (print-pass str) (cprintf (format "  test-~a: pass\n" str)))
(define (print-fail str) (cprintf (format "  test-~a: fail\n" str)))

(define-syntax-parse-rule (with-allocation ((vars types) ...) body-stmt ...)
  (stmt-expr
   (expr-let ([vars (op-alloca (expr-etype types)) (ll-type-pointer types)] ...)
             (stmt-block body-stmt ...)
             (expr-void))))
