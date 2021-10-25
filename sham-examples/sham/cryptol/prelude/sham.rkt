#lang racket

(require sham/ir
         sham/jit
         sham/md
         (prefix-in ll- sham/llvm/ir))
(provide (all-defined-out))

(define primitive-true (ui1 1))
(define primitive-false (ui1 0))
(define test-function-type (ll-type-function #f ll-void))
(define (basic-function-type . args) (ll-make-type-function args #f i64))
(define (basic-function-body body-stmt) (stmt-block body-stmt (stmt-return (ui64 0))))

(define (store-basic-val! val result)
  (stmt-expr (op-store! val result)))

;; (define (basic-function-type args result)
;;   (ll-make-type-function (append args (list (ll-type-pointer result))) #f i64))

(define (print-yay str)
  (stmt-expr (expr-app 'printf (ll-val-string (format "test-passed-~a\n" str))))
  ;; (stmt-void)
  )

(define (print-nay str)
  (stmt-expr (expr-app 'printf (ll-val-string (format "test-failed-~a\n" str))))
  ;; (stmt-void)
  )

(define-syntax (with-allocation stx)
  (syntax-case stx ()
    [(_ ((vars types) ...) body-stmt ...)
     #`(stmt-expr
        (expr-let ([vars (op-alloca (expr-etype types)) (ll-type-pointer types)] ...)
                  (stmt-block body-stmt ...)
                  (expr-void)))]))
