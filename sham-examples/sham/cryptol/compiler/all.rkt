#lang racket

(require syntax/strip-context
         sham/sam/pretty)

(require "compiler.rkt"
         "sham.rkt"
         "transform.rkt"
         "stx-to-cry-ast.rkt"
         "utils.rkt"
         "ctxt.rkt"
         "debug.rkt"
         "../prelude/cryptol.rkt"
         "../ir/to-cry-ir.rkt")

(provide (all-defined-out))

(define primitive-typeofs-asts (stx-to-cry-ast primitive-typeofs-stx))
(define prelude-defs-asts (stx-to-cry-ast prelude-defs-stx))

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

(define sham-top
  (λ (stxs)
    (debug (printf "compiling:\n") (for ([s stxs]) (pretty-print-ast s)) (newline))

    (define cry-asts (map stx-to-cry-ast stxs))

    (debug (printf "cry-ast:\n") (for ([ca cry-asts]) (pretty-print (pretty-cry ca)) (newline)))

    (define-values (res-stxs ctxt)
      (parameterize ([current-compiler sham-compiler])
        (compile-cry cry-asts)))
    (define result-syntax
      (strip-context
       #`(module default racket
           #,@require-syntax
           (define cryptol-module
             (def-module 'cryptol-module
               #,@primitive-defs
               #,res-stxs
               #,@(map env-var-val (unbox (cc-lifts ctxt)))))
           #,@extra-syntax
           )))
    (debug (parameterize ([pretty-print-columns 120]) (pretty-print (syntax->datum result-syntax))))
    result-syntax))
(define cry-ir
  (λ (stxs)
    (define cry-asts (map stx-to-cry-ast stxs))
    (debug (printf "cry-ast:\n") (for ([ca cry-asts]) (pretty-print (pretty-cry ca)) (newline)))
    (define cry-ir (to-cry-ir cry-asts primitive-typeofs-asts prelude-defs-asts))
    (printf "cry-ir: \n")
    (parameterize ([pretty-print-columns 120]) (pretty-print (map pretty-print-ast cry-ir)))
   (strip-context
     #`(module default racket
         ;; #,@result-stxs
         )) ))
;; maps to functions that take a list of stxs and return the final syntax for a file
(define compiler-maps (make-hash `((test . ,(create-test-top test-compiler))
                                   (sham . ,sham-top)
                                   (ir . ,cry-ir))))

(define (get-compiler key) (hash-ref compiler-maps key))
