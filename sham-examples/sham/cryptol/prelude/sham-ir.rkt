#lang racket

(require sham/ir
         sham/jit
         sham/md
         syntax/parse/define
         (prefix-in ll- sham/llvm/ir))
(provide (all-defined-out))

(define bit-type i1)
(define primitive-true (ui1 1))
(define primitive-false (ui1 0))
(define test-function-type (ll-type-function #f ll-void))
(define (basic-function-type . args) (ll-make-type-function args #f i64))
(define (ptr-type t) (ll-type-pointer t))
(define (alloca t) (op-alloca (expr-etype t)))
(define (arr-alloc t len) (op-array-alloca (expr-etype t) len))

(define (store-val! val ptr) (stmt-expr (op-store! val ptr)))

(define-syntax-parse-rule (cprintf str args ...)
  (stmt-expr (expr-app 'printf (ll-val-string str) args ...)))

(define (print-pass str) (cprintf (format "  test-~a: pass\n" str)))
(define (print-fail str) (cprintf (format "  test-~a: fail\n" str)))

(define-syntax-parse-rule (alloca-let ((vars types) ...) body-stmts ...)
  (stmt-expr
   (expr-let ([vars (alloca types) (ptr-type types)] ...)
             (stmt-block body-stmts ...)
             (expr-void))))

(define-syntax-parse-rule (stmt-let ((vars vals types) ...) body-stmts ...)
  (stmt-expr
   (expr-let ([vars vals types] ...)
             (stmt-block body-stmts ...)
             (expr-void))))

(define-syntax-parse-rule (basic-function-body vvt body-stmts ...)
  (stmt-let vvt body-stmts ... (stmt-return (ui64 0))))

(define-syntax-parse-rule (basic-i64-loop [itr start end] stmts ...)
  (stmt-let [(itr start i64)]
            (stmt-while (op-icmp-ule (expr-ref itr) end)
                        stmts ...
                        (stmt-set! itr (op-add itr (ui64 1))))))

;; sequence
(define (seq-type it) (ll-type-struct i64 i64 (ll-type-pointer it)))
(define (seq-len-ptr s) (op-gep s (ui32 0) (ui32 0)))
(define (seq-upto-ptr s) (op-gep s (ui32 0) (ui32 1)))
(define (seq-array-ptr-ptr s) (op-gep s (ui32 0) (ui32 2)))
(define (seq-array-ptr s) (op-load (seq-array-ptr-ptr s)))
(define (seq-index-ptr s idx) (op-gep (seq-array-ptr s) (op-int-cast idx (expr-etype i32))))

(define (lazy-seq-func-type clos-type elem-type) (ll-type-function (seq-type elem-type) i64 clos-type #f elem-type))
(define lazy-func-seq (expr-ref 0))
(define lazy-func-index (expr-ref 1))
(define lazy-func-clos (expr-ref 2))
;; (define lazy-func-upto (op-load (lazy-seq-upto-ptr lazy-func-seq)))

;; closure
(define (clos-type . locals) (ll-make-type-struct (map ll-make-type-pointer locals)))
(define (clos-index-ptr c i) (op-gep c (ui32 0) (ui32 i)))
(define (clos-index-val c i) (op-load (clos-index-ptr c i)))
(define (lazy-func-clos-val idx) (op-load (clos-index-ptr lazy-func-clos idx)))

;; tuple
(define (tup-type . ts) (ll-make-type-struct ts))
(define (tup-index-ptr t i) (op-gep t (ui32 0) i))