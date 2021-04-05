#lang racket
(require (for-syntax syntax/parse racket/syntax racket/match)
         sham/llvm/ir/simple)

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
          (cons #`(define #,curr (llt-ref (quote #,curr))) (rec curr next))]
         [(p (cons curr next))
          (cons #`(define #,curr (llt-pointer #,p)) (rec curr next))]
         [(p empty) empty]))
     #`(begin #,@(apply append (map (λ (n) (rec #f n)) name-list)))]))

(define-ref-types i1 i8 i16 i32 i64 f32 f64 void)
