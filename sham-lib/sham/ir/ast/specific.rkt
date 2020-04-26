#lang racket

(require sham/ast/core
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple))

(define-simple-macro (define-alias-with-ptr-types names:id ...)
  #:with ((name-list ...) ...)
  (map (λ (n) (map (λ (i) (format-id n "~a~a" n i))
                   (build-list 4 (λ (j) (make-string j #\*)))))
       (syntax->list #`(names ...)))
  (begin (define-llvm-alias name-list ...) ...))
(define-alias-with-ptr-types i1 i8 i16 i32 i64 f32 f64 void)

(define-simple-macro (define-int-const-alias widths ...)
  #:with (is ...) (map (λ (t) (format-id t "i~a" t)) (syntax->list #`(types ...)))
  #:with (us ...) (map (λ (t) (format-id t "ui~a" t)) (syntax->list #`(types ...)))
  #:with (ss ...) (map (λ (t) (format-id t "si~a" t)) (syntax->list #`(types ...)))
  (begin (define ui llvm-const-ui)
         (define (us v) (ui v is)) ...
         (define si llvm-const-si)
         (define (ss v) (si v is)) ...))
(define-int-const-alias 1 8 16 32 64)

(define-simple-macro (define-float-const-alias widths ...)
  #:with (fn ...) (map (λ (w) (format-id w "fl~a" w)) (syntax->list #`(widths ...)))
  #:with (ft ...) (map (λ (w) (format-id w "f~a" w)) (syntax->list #`(widths ...)))
  (begin (define fl llvm-const-fl)
         (define (fn v) (fl v ft)) ...))
(define-float-const-alias 32 64)

(define v4i32 (tvec i32 4))
(define v4i64 (tvec i64 4))
(define v4f32 (tvec f32 4))
(define v4f64 (tvec f64 4))
