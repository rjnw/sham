#lang racket

(require sham/env/infos
         sham/ast/core
         "callc.rkt")
(provide (all-defined-out))
(define (add-stmt-llvm-md sham-ast llvm-value)
  (define md (sham:ast-metadata sham-ast))
  llvm-value)
(define (add-expr-llvm-md sham-ast llvm-value)
  (define md (sham:ast-metadata sham-ast))
  (cond [(sham:ast:expr:app? sham-ast)
         (when (app-info-get-call-conv md)
           (set-instruction-call-conv! llvm-value (app-info-get-call-conv md)))])
  llvm-value)

(define (add-rator-llvm-md sham-ast llvm-value)
  (define md (sham:ast-metadata sham-ast))
  (cond [(app-info-get-call-conv md)
         (set-instruction-call-conv! llvm-value (app-info-get-call-conv md))])
  llvm-value)

(define (add-function-llvm-md sham-ast llvm-value)
  (define md (sham:def-info sham-ast))
  (cond [(function-info-get-call-conv md)
         (set-function-call-conv! llvm-value (function-info-get-call-conv md))])
  llvm-value)

(define (add-local-function-info info llvm-value)
  (cond [(function-info-get-call-conv info)
         (set-instruction-call-conv! llvm-value (function-info-get-call-conv info))])
  llvm-value)
