#lang racket

(require (prefix-in llvm- sham/llvm/ir/internals)
         (for-syntax racket/syntax syntax/parse)
         sham/ir/simple)


(define-syntax (define-basic-ops stx)
  (define (do-op op)
    (with-syntax ([op-syn (datum->syntax stx op)]
                  [md-syn (generate-temporary `md)])
      #`(define-syntax (op-syn stx)
          (syntax-parse stx
            [(_ (~optional (~seq #:md md-syn)) args (... ...))
             (syntax/loc stx (expr-op #,(datum->syntax stx `(~? (~@ #:md ,#'md-syn))) (quote op-syn) args (... ...)))]))))
  (syntax-case stx ()
    [(_ ops-reference)
     #`(begin #,@(map do-op (syntax-local-value #'ops-reference)))]))

(module* ops #f
  (provide (all-defined-out))
  (define-basic-ops llvm-basic-ops))
