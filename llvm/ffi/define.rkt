#lang racket

(require  syntax/parse/define
  racket/splicing

  (only-in ffi/unsafe get-ffi-obj)
  "lib.rkt")

(provide
  define-llvm
  define-llvm-multiple)

(define-syntax (define-ffi-definer stx)
 (syntax-case stx ()
  ((_ name lib)
   #'(define-syntax (name stx)
       (syntax-case stx ()
        ((_ id type #:c-id c-id)
         #'(define id (get-ffi-obj 'c-id lib type)))
        ((_ id type)
         #'(define id (get-ffi-obj 'id lib type))))))))

(define-ffi-definer define-llvm llvm-lib)

(define-syntax define-llvm-multiple
  (syntax-parser
   ((_ (name:id ...) . rest)
    #'(begin
        (define-llvm name . rest) ...))))
