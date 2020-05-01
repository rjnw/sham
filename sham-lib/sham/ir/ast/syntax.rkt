#lang racket

(require sham/ir/ast/core
         sham/ir/ast/simple
         sham/llvm/ir/ast
         sham/llvm/ir/md
         (prefix-in llvm- sham/llvm/ir/simple))

(require (for-syntax racket/syntax syntax/parse))

(provide (all-defined-out))

;; stmt
(define set!^ s-set!)
(define if^ s-if)
(define continue^ s-continue)
(define break^ s-break)
(define svoid^ s-void)
(define expr^ s-expr)
(define return^ s-return)
(define return-void^ s-return-void)

(define (stmt-block stmts)
  (s-block
   (append-map
    (λ (v) (cond [(sham:ast:expr? v) (s-expr v)]
                 [(sham:ast:stmt:block? stmts) stmts]
                 [(sham:ast:stmt? v) v]
                 [(list? v) v]
                 [else (error "block expects a stmt/expr given: " v)]))
    stmts)))

(define (block^ . stmts)
  (if (eq? (length stmts) 1)
      (car stmts)
      (stmt-block stmts)))

(define (while^ expr . stmts)
  (s-while expr (stmt-block stmts)))

(define-syntax (let^ stx)
  (syntax-parse stx
    [(_ ([arg (~optional val) (~datum :) typ] ...) s:expr ... e:expr)
     #`(let ([arg (e-ref (quasiquote arg))] ...)
         (e-let `(arg ...) (list (~? val #f) ...) (list typ ...)
                (block^ s ...)
                e))]))

(define-syntax (slet^ stx)
  (syntax-parse stx
    [(_ args ...) #`(s-expr (let^ args ...))]))

(define-syntax (switch^ stx)
  (syntax-parse stx
    [(_ v:expr [check:expr body:expr ...] ... default)
     #`(s-switch v (list check ...) (list (block^ body ...) ...) default)]))

(define-syntax (label^ stx)
  (syntax-parse stx
    [(_ name:id s:expr ...) #`(s-label (quote name) (block^ s ...))]
    [(_ name:expr s:expr ...) #`(s-label name (block^ s ...))]))

;; expr
(define ref^ e-ref)
(define op^ e-op)
(define-syntax (access^ stx)
  (syntax-parse stx
    [(_ struct-field value) #`(e-access struct-field value)]
    [(_ struct-name:id field-name:id value) #`(e-access (cons struct-name field-name) value)]))
(define evoid^ e-void)
(define etype e-etype)

(define (app^ rator #:flags (flags #f) . rands)
  (match rator
    [(? sham:ast:rator?) (e-op rator flags rands)]
    [(sham:ast:expr:ref md v) (e-op (r-reference v) flags rands)]
    [(? symbol?) (e-op (r-reference rator) flags rands)]
    [(? string?)
     (if (type? (car rands))
         (e-op (r-intrinsic rator (car rands)) flags (cdr rands))
         (error 'sham:ir "expected type for intrinsic as first argument, ~a/~a" rator (car rands)))]
    [(? procedure?) (apply rator rands)]
    [else (error 'sham:ir "expected rator for app^ given: ~a" rator)]))

;; defs
(define-syntax (function^ stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:info info))
        (name:expr (args:id (~datum :) arg-types:expr) ... (~datum :) ret-type:expr)
        body:expr ...)
     #:with (arg-nums ...) (build-list (length (syntax->list #`(args ...)))
                                 (λ (i) #`#,i))
     #:with (arg-type-names ...) (generate-temporaries #`(args ...))
     #`(let ([arg-type-names arg-types] ...)
         (d-function (~? info (empty-function-info))
                     (quasiquote name) (t-function (list arg-type-names ...) ret-type)
                     (slet^ ([args (llvm-val-param arg-nums) : arg-type-names] ...)
                            body ...
                            (e-void))))]))

(define-syntax (struct^ stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:info info)) name:id (field-name:id field-type:expr) ...)
     #`(d-struct (~? info (empty-struct-info)) (quasiquote name) `(field-name ...) (list field-type ...))]))

(define-syntax (module^ stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:info info)) name:id (defs ...))
     #`(d-module (~? info (empty-module-info)) (quasiquote name) (list defs ...))]))
