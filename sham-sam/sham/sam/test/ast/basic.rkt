#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))

(provide (all-defined-out))

(define-ast basic
  (expr
   [zero]
   [seq (a:expr b:expr c:expr)]
   [rep (a:expr ...)]
   [sr1 (a:expr ... b:expr)]
   [sr2 (a:expr (b:expr ...))])
  ;; (numeric expr
  ;;          [bit])
  ;; #:format (#f - #f - -)
  )
