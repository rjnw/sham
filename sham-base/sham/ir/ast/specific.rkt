#lang racket

(require sham/ir/ast/core
         sham/ir/ast/simple
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple)
         (prefix-in llvm- sham/llvm/ir/specific))

(require syntax/parse/define
         (for-syntax racket/syntax))

(provide (except-out (all-defined-out)
                     define-llvm-alias
                     define-alias-with-ptr-types
                     define-int-const-alias
                     define-float-const-alias))

(define-simple-macro (define-llvm-alias names:id ...)
  #:with (llvm-names ...) (map (λ (n) (format-id n "llvm-~a" n))
                               (syntax->list #`(names ...)))
  (begin (define names llvm-names) ...))

(define-simple-macro (define-alias-with-ptr-types names:id ...)
  #:with ((name-list ...) ...)
  (map (λ (n) (map (λ (i) (format-id n "~a~a" n i))
                   (build-list 4 (λ (j) (make-string j #\*)))))
       (syntax->list #`(names ...)))
  (begin (define-llvm-alias name-list ...) ...))
(define-alias-with-ptr-types i1 i8 i16 i32 i64 f32 f64 void)

(define-simple-macro (define-int-const-alias widths ...)
  #:with (is ...) (map (λ (t) (format-id t "i~a" (syntax->datum t))) (syntax->list #`(widths ...)))
  #:with (us ...) (map (λ (t) (format-id t "ui~a" (syntax->datum t))) (syntax->list #`(widths ...)))
  #:with (ss ...) (map (λ (t) (format-id t "si~a" (syntax->datum t))) (syntax->list #`(widths ...)))
  (begin (define ui llvm-val-ui)
         (define (us v) (ui v is)) ...
         (define si llvm-val-si)
         (define (ss v) (si v is)) ...))
(define-int-const-alias 1 8 16 32 64)

(define-simple-macro (define-float-const-alias widths ...)
  #:with (fn ...) (map (λ (w) (format-id w "fl~a" (syntax->datum w))) (syntax->list #`(widths ...)))
  #:with (ft ...) (map (λ (w) (format-id w "f~a" (syntax->datum w))) (syntax->list #`(widths ...)))
  (begin (define fl llvm-val-fl)
         (define (fn v) (fl v ft)) ...))
(define-float-const-alias 32 64)

(define v4i32 (t-vector i32 4))
(define v4i64 (t-vector i64 4))
(define v4f32 (t-vector f32 4))
(define v4f64 (t-vector f64 4))
