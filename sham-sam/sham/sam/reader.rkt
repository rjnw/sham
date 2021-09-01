#lang racket

(require
 (for-syntax
  racket/syntax
  syntax/parse
  "syntax/spec.rkt"
  "syntax/parser.rkt"))

;; (provide build-reader)

#;(define-syntax (build-reader stx)
  (syntax-parse stx
    [(_ reads:reader-spec)
     (define parsers (build-parsers (attribute reads.spec)))
     parsers
     ;; #'(void)
     ]))
