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
(define basic-sequence-type (ll-type-struct i64 i8*))
;; (define (allocate-sequence dim subtype))
(define (store-basic-val! val ptr)
  (stmt-expr (op-store! val ptr)))

(define-syntax-parse-rule (cprintf str args ...)
  (stmt-expr (expr-app 'printf (ll-val-string str) args ...)))

(define (print-pass str) (cprintf (format "  test-~a: pass\n" str)))
(define (print-fail str) (cprintf (format "  test-~a: fail\n" str)))

(define-syntax-parse-rule (stmt-let ((vars vals types) ...) body-stmts ...)
  (stmt-expr
   (expr-let ([vars vals types] ...)
             (stmt-block body-stmts ...)
             (expr-void))))

(define-syntax-parse-rule (basic-function-body vvt body-stmts ...)
  (stmt-let vvt body-stmts ... (stmt-return (ui64 0)))
  ;; (stmt-expr
  ;;  (expr-let ([vars vals types] ...)
  ;;            (stmt-block body-stmts ... (stmt-return (ui64 0)))
  ;;            (expr-void)))
  )
