#lang racket

(provide (all-defined-out))

(define current-sham-module (make-parameter #f))
(define compile-options (make-parameter '()))
(define common-function-info (make-parameter #f))
(define sham-diagnose (make-parameter #f))
(define sham-debug (make-parameter #f))
