#lang racket
(require (for-syntax syntax/parse racket/syntax racket/match)
         sham/llvm/ir/simple
         sham/llvm/ir/op)

(provide (except-out (all-defined-out)
                     define-ref-types))

(define-syntax (define-ref-types stx)
  (syntax-parse stx
    [(_ names:id ...)
     (define name-list (map (λ (n) (map (λ (i) (format-id n "~a~a" n i))
                                        (build-list 4 (λ (j) (make-string j #\*)))))
                            (syntax->list #`(names ...))))
     (define (rec prev l)
       (match* (prev l)
         [(#f (cons curr next))
          (cons #`(define #,curr (type-ref (quote #,curr))) (rec curr next))]
         [(p (cons curr next))
          (cons #`(define #,curr (type-pointer #,p)) (rec curr next))]
         [(p empty) empty]))
     #`(begin #,@(apply append (map (λ (n) (rec #f n)) name-list)))]))

(define-ref-types i1 i8 i16 i32 i64 f32 f64 void)

;; (define-simple-macro (define-llvm-alias names:id ...)
;;   #:with (llvm-names ...) (map (λ (n) (format-id n "llvm-~a" n))
;;                                (syntax->list #`(names ...)))
;;   (begin (define names llvm-names) ...))

;; (define-simple-macro (define-alias-with-ptr-types names:id ...)
;;   #:with ((name-list ...) ...)
;;   (map (λ (n) (map (λ (i) (format-id n "~a~a" n i))
;;                    (build-list 4 (λ (j) (make-string j #\*)))))
;;        (syntax->list #`(names ...)))
;;   (begin (define-llvm-alias name-list ...) ...))
;; (define-alias-with-ptr-types i1 i8 i16 i32 i64 f32 f64 void)

;; (define-simple-macro (define-int-const-alias widths ...)
;;   #:with (is ...) (map (λ (t) (format-id t "i~a" (syntax->datum t))) (syntax->list #`(widths ...)))
;;   #:with (us ...) (map (λ (t) (format-id t "ui~a" (syntax->datum t))) (syntax->list #`(widths ...)))
;;   #:with (ss ...) (map (λ (t) (format-id t "si~a" (syntax->datum t))) (syntax->list #`(widths ...)))
;;   (begin (define (ui v t) (llvm-val-ui v t))
;;          (define (us v) (llvm-val-ui v is)) ...
;;          (define (si v t) (llvm-val-si v t))
;;          (define (ss v) (si v is)) ...))
;; (define-int-const-alias 1 8 16 32 64)

;; (define-simple-macro (define-float-const-alias widths ...)
;;   #:with (fn ...) (map (λ (w) (format-id w "fl~a" (syntax->datum w))) (syntax->list #`(widths ...)))
;;   #:with (ft ...) (map (λ (w) (format-id w "f~a" (syntax->datum w))) (syntax->list #`(widths ...)))
;;   (begin (define (fl v t) (llvm-val-fl v t))
;;          (define (fn v) (fl v ft)) ...))
;; (define-float-const-alias 32 64)
