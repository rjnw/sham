#lang racket

(require sham/llvm/ir/md
         sham/llvm/ir/context
         sham/ir/context)
(provide (all-defined-out))

(define global-sham-context (sham-context (global-llvm-context)))

(define current-sham-module (make-parameter #f))
(define current-function-info (make-parameter (empty-function-info)))

(define sham-compile-options (make-parameter '()))
(define sham-build-options (make-parameter '()))

(define sham-diagnose (make-parameter #f))
(define sham-debug (make-parameter #f))
