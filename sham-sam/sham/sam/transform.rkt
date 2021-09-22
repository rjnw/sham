#lang racket
(require (for-syntax syntax/parse))
(require
 "runtime/transform.rkt"
 (for-syntax "syntax/transform.rkt"
             (submod "syntax/class.rkt" compiler)))

(provide (all-from-out "runtime/transform.rkt")
         (all-defined-out))

(define-syntax (define-transform stx)
  (syntax-parse stx
    [(_ cmplr:compiler-spec)
     (define-values (cmplr-stx cmplr-spec) (build-transform-syntax (attribute cmplr.spec)))
     ;; (match-define (cmplr header groups info) cmplr-spec)

     (define stx
       #`(begin
           ;; (define-syntax #,(cmplr:header-id header) #,(compiler-syntax-storage cmplr-spec))
           #,@cmplr-stx))
     stx]))

(define-syntax rkt-syntax (rkt-syntax-cmplr #f))
