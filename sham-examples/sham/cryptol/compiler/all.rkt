#lang racket

(require syntax/strip-context
         sham/sam/pretty)

(require "compiler.rkt"
         "sham.rkt"
         "transform.rkt"
         "stx-to-cry-ast.rkt"
         "utils.rkt"
         "ctxt.rkt"
         "debug.rkt")

(provide (all-defined-out))

(define (create-test-top cmplr)
  (λ (stxs)
    (debug (printf "compiling:\n") (for ([s stxs]) (pretty-print s)))

    (define cry-asts (map stx-to-cry-ast stxs))

    (debug (printf "cry-ast:\n") (for ([ca cry-asts]) (pretty-print (pretty-cry ca)) (newline)))

    (define-values (res-stxs ctxt)
      (parameterize ([current-compiler cmplr])
        (compile-cry cry-asts)))
    (debug (printf "result:\n~a\n" res-stxs))
    (pretty-print (unbox (cc-lifts ctxt)))

    (strip-context
     #`(module default racket
         ;; #,@result-stxs
         ))))

;; maps to functions that take a list of stxs and return the final syntax for a file
(define compiler-maps (make-hash `((test . ,(create-test-top test-compiler))
                                   (sham . ,(create-test-top sham-compiler)))))

(define (get-compiler key) (hash-ref compiler-maps key))
