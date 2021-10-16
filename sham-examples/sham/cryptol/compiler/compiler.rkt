#lang racket
(require (for-syntax racket/syntax))
(provide (all-defined-out))

(define-for-syntax compiler-functions
  #`(
     def-val
     def-test

     tests

     function-arg
     function-result

     expr-app
     expr-app-primitive
     expr-cond
     expr-bind
     expr-var
     expr-tvar

     error-msg

     tuple-literal
     integer-literal
     char-literal
     zero-literal

     tuple-index
     sequence-index

     sequence-basic
     sequence-enum
     sequence-string
     sequence-comp
     sequence-var

     type
     type-sequence
     type-int
     type-tuple))

(define-syntax (create-compiler-struct stx)
  (syntax-case stx ()
    [(_ cid) #`(struct cid #,compiler-functions)]))
(create-compiler-struct compiler)

(define-syntax (create-test-compiler stx)
  #`(compiler
     #,@(for/list ([f (syntax-e compiler-functions)]) #`(λ (ctxt . args) `(#,f ,@args)))))

(define test-compiler (create-test-compiler))

(define current-compiler (make-parameter test-compiler))

(define-syntax (create-compiler-functions stx)
  #`(begin
      #,@(for/list ([f (syntax-e compiler-functions)])
           #`(define-syntax (#,(format-id stx "compile-~a" f) stx)
               (syntax-case stx ()
                 [(_ args (... ...) ctxt)
                  #'((#,(format-id f "compiler-~a" f) (current-compiler))
                     ctxt args (... ...))])))))

(create-compiler-functions)
