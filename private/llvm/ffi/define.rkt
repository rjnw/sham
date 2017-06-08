#lang racket

(require
 ffi/unsafe
 ffi/unsafe/define
 syntax/parse/define
 "lib.rkt")

(provide
  define-llvm
  define-llvm-multiple)

(define-ffi-definer define-llvm llvm-lib)

(define-syntax define-llvm-multiple
  (syntax-parser
   ((_ (name:id ...) . rest)
    #'(begin
        (define-llvm name . rest) ...))))
