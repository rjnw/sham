#lang racket

(require (for-syntax syntax/parse
                     racket/match
                     racket/list
                     racket/pretty
                     "syntax/spec.rkt"
                     "common/ooo.rkt"
                     "common/generics.rkt"
                     (submod "syntax/private/spec.rkt" compiler)
                     (submod "syntax/private/syntax-class.rkt" compiler)
                     (submod "generics.rkt" compiler))
         racket/stxparam)
(require (for-template (prefix-in rkt: racket)))
(provide (all-defined-out))

(define-syntax-parameter compile (make-rename-transformer #'rkt:compile))
(define-syntax-parameter ^ (make-rename-transformer #'rkt:^))

(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ cmplr:compiler-spec)
     (define-values (cmplr-stx cmplr-spec) (build-compiler-syntax (attribute cmplr.spec)))
     ;; (match-define (cmplr header groups info) cmplr-spec)
     (define stx
       #`(begin
           ;; (define-syntax #,(cmplr:header-id header) #,(compiler-syntax-storage cmplr-spec))
           #,@cmplr-stx))
     (pretty-print stx)
     #`42]))
