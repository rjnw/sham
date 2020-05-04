#lang racket

(require sham/md
         sham/ir/ast/core
         sham/ir/ast/simple
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple))

(require (for-syntax racket/syntax syntax/parse)
         syntax/parse/define)

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
   (flatten
    (for/list ([s stmts])
      (cond [(sham:ast:expr? s) (s-expr s)]
            [(sham:ast:stmt:block? s) (sham:ast:stmt:block-stmts s)]
            [(sham:ast:stmt? s) s]
            [(list? s) s]
            [(llvm:ast:instruction? s) s]
            [else (error "block expects a stmt/expr given: " s)])))))

(define (block^ . stmts)
  (if (eq? (length stmts) 1)
      (car stmts)
      (stmt-block stmts)))

(define (while^ expr . stmts)
  (s-while expr (stmt-block stmts)))

(define-syntax (let^ stx)
  (syntax-parse stx
    [(_ ([arg (~optional val) (~datum :) typ] ...) s:expr ... e:expr)
     #`(let ([arg (e-ref (gensym (quasiquote arg)))] ...)
         (e-let (list arg ...) (list (~? val #f) ...) (list typ ...)
                (block^ s ...)
                e))]))

(define-simple-macro (slet^ args ...)
  (s-expr (let^ args ... (e-void))))

(define-simple-macro (switch^ v:expr [check:expr body:expr ...] ... default)
  (s-switch v (list check ...) (list (block^ body ...) ...) default))

(define-simple-macro (label^ name:id stmt ...)
  (s-label `name (block^ stmt ...)))
(define-simple-macro (label-jump^ name:id)
  (llvm-ast-bru `name))

;; expr
(define ref^ e-ref)
(define op^ e-op)
(define-syntax (access^ stx)
  (syntax-parse stx
    [(_ struct-field value) #`(e-access struct-field value)]
    [(_ struct-name:id field-name:id value) #`(e-access (cons struct-name field-name) value)]))
(define evoid^ e-void)
(define etype e-etype)

(define (app^ rator #:flags (flags #f) . rands) (e-app rator flags rands))

;; defs
(define-simple-macro (function-body^ [(args (~datum :) arg-types) ...] body ...)
  #:with (arg-nums ...) (build-list (length (syntax->list #`(args ...))) (λ (i) #`#,i))
  (slet^ ([args (llvm-val-param arg-nums) : arg-types] ...)
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
