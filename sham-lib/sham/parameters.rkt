#lang racket

(require sham/md
         sham/llvm/ir/context
         sham/ir/context)

(provide (all-defined-out))

(define sham-global-context (make-parameter (sham-context (global-llvm-context))))

(define sham-current-env (make-parameter #f))
(define sham-current-function-md (make-parameter (empty-function-md)))

(define sham-compile-options (make-parameter '()))
(define sham-build-options (make-parameter '()))

(define sham-diagnose (make-parameter #f))
(define sham-debug (make-parameter #f))
