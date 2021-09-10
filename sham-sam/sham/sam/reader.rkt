#lang racket

(require
 (for-syntax
  racket/syntax
  syntax/parse
  (submod "syntax/spec.rkt" reader)
  "syntax/reader.rkt"))

 (provide build-reader)

(define-syntax (build-reader stx)
  (syntax-parse stx
    [(_ reads:reader-spec)
     (define readers (build-readers (attribute reads.spec)))
     parsers
     ;; #'(void)
     ]))
