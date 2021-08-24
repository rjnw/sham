#lang racket
(require (for-syntax syntax/parse))
(require
 "runtime/compiler.rkt"
 (for-syntax "syntax/compiler.rkt"
             (submod "syntax/class.rkt" compiler)))

(provide (all-from-out "runtime/compiler.rkt")
         (all-defined-out))

(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ cmplr:compiler-spec)
     (define-values (cmplr-stx cmplr-spec) (build-compiler-syntax (attribute cmplr.spec)))
     ;; (match-define (cmplr header groups info) cmplr-spec)

     (define stx
       #`(begin
           ;; (define-syntax #,(cmplr:header-id header) #,(compiler-syntax-storage cmplr-spec))
           #,@cmplr-stx))
     stx]))
