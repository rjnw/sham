#lang racket
(require (for-syntax racket/syntax
                     syntax/parse))
(require racket/trace)
(provide (all-defined-out))

(define-for-syntax compiler-functions
  #`(
     ;; create a new internal context, once for each val and test
     internal-def-context

     def-val
     def-test
     def-test-results

     tests

     function-arg
     function-result

     where-val-env
     where-val-res
     where-val

     expr-result

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
     sequence-index-result

     sequence-basic
     sequence-enum
     sequence-string

     sequence-comp-var
     sequence-comp-result
     sequence-comp-index
     sequence-comp

     type
     type-sequence
     type-int
     type-tuple))

(define-syntax (create-compiler-struct stx)
  (syntax-case stx ()
    [(_ cid) #`(struct cid #,compiler-functions)]))
(create-compiler-struct compiler)


(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ name:id ((fname:id . fargs) . fbodys) ...)
     (define given-fs
       (map list
            (syntax->datum #`(fname ...))
            (syntax-e #`(fargs ...))
            (syntax-e #`(fbodys ...))))
     #`(define name
         (compiler
          #,@(for/list ([f (syntax-e compiler-functions)])
               (cond
                 [(assoc (syntax->datum f) given-fs)
                  =>
                  (λ (given) (quasisyntax/loc f (lambda #,(cadr given) #,@(caddr given))))]
                 [else #`(λ (ctxt . args) `(TODO #,f ,@args))]))))]))

;; (define test-compiler (create-test-compiler))

(define-compiler test-compiler)
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
