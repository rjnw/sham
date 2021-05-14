#lang racket

(require sham/md
         sham/ir/ast
         sham/ir/simple
         (prefix-in ll- sham/llvm/ir/simple))

(require (for-syntax racket/syntax syntax/parse)
         syntax/parse/define)

(provide (all-defined-out))
;; TODO for maintaing backward compatibility with "Sham: A DSL for Fast DSLs" paper o/w just use simple

;; stmt
(define set!^ stmt-set!)
(define if^ stmt-if)
(define continue^ stmt-continue)
(define break^ stmt-break)
(define svoid^ stmt-void)
(define expr^ stmt-expr)
(define return^ stmt-return)
(define return-void^ stmt-return-void)

(define (stmt-block stmts)
  (stmt-block
   (flatten
    (for/list ([s stmts])
      (cond [(sham:expr? s) (stmt-expr s)]
            [(sham:stmt:block? s) (sham:stmt:block-stmts s)]
            [(sham:stmt? s) s]
            [(list? s) s]
            [(ll-inst? s) s]
            [else (error "block expects a stmt/expr given: " s)])))))

(define (block^ . stmts)
  (if (eq? (length stmts) 1)
      (car stmts)
      (stmt-block stmts)))

(define (while^ expr . stmts)
  (stmt-while expr (stmt-block stmts)))

(define-syntax (let^ stx)
  (syntax-parse stx
    [(_ ([arg (~optional val) (~datum :) typ] ...) s:expr ... e:expr)
     #`(let ([arg (expr-ref (gensym (quasiquote arg)))] ...)
         (expr-let (list arg ...) (list (~? val #f) ...) (list typ ...)
                (block^ s ...)
                e))]))

(define-simple-macro (slet^ args ...)
  (stmt-expr (let^ args ... (expr-void))))

(define-simple-macro (switch^ v:expr [check:expr body:expr ...] ... default)
  (stmt-switch v (list check ...) (list (block^ body ...) ...) default))

(define-simple-macro (label^ name:id stmt ...)
  (stmt-label `name (block^ stmt ...)))
(define-simple-macro (label-jump^ name:id)
  (ll-bru `name))

;; expr
(define ref^ expr-ref)
(define op^ expr-op)
(define-syntax (access^ stx)
  (syntax-parse stx
    [(_ struct-field value) #`(e-access struct-field value)]
    [(_ struct-name:id field-name:id value) #`(e-access (cons struct-name field-name) value)]))
(define evoid^ expr-void)
(define etype expr-etype)

(define (app^ rator #:flags (flags #f) . rands) (expr-app rator flags rands))

;; defs
(define-simple-macro (function-body^ [(args (~datum :) arg-types) ...] body ...)
  #:with (arg-nums ...) (build-list (length (syntax->list #`(args ...))) (λ (i) #`#,i))
  (slet^ ([args (ll-val-param arg-nums) : arg-types] ...)
         body ...))

(define-syntax (function^ stx)
  (syntax-parse stx
    [(_ (~optional (~seq (~or (~datum #:md)
                              (~datum #:metadata)) md))
        (name:expr (args:id (~datum :) arg-types:expr) ... (~datum :) ret-type:expr)
        body:expr ...)
     #:with (arg-type-names ...) (generate-temporaries #`(args ...))
     #`(let ([arg-type-names arg-types] ...)
         (d-function (~? md (empty-function-md))
                     (quasiquote name) (t-function (list arg-type-names ...) ret-type)
                     (function-body^ [(args : arg-type-names) ...] body ...)))]))

(define-simple-macro (efunction^ header ... body)
  (function^ header ... (return^ body)))

(define-syntax (struct^ stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:md md)) name:id (field-name:id field-type:expr) ...)
     #`(d-struct (~? md (empty-struct-md)) (quasiquote name) `(field-name ...) (list field-type ...))]))

(define-syntax (module^ stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:md md)) name:id (defs ...))
     #`(d-module (~? md (empty-module-md)) (quasiquote name) (list defs ...))]))
