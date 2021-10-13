#lang racket

(require syntax/strip-context
         sham/sam/pretty)

(require "compiler.rkt"
         "transform.rkt"
         "stx-to-cry-ast.rkt"
         "utils.rkt")

(provide (all-defined-out))

  ;; (define (get-ast stx)
  ;;   (println "read-syntax:") (pretty-print (syntax->datum stx))
  ;;   (define cry-ast (stx-to-cry-ast stx))
  ;;   (println "parsed-cry:") (pretty-print-ast cry-ast) (newline)
  ;;   ;; (define rkt-stx (cry-ast-to-rkt-stx cry-ast))
  ;;   ;; (pretty-print (syntax->datum rkt-stx))
  ;;   ;; rkt-stx
  ;;   cry-ast
  ;;   )

(define (create-test-top cmplr)
  (λ (stxs)
    (debug (printf "compiling:\n") (for ([s stxs]) (pretty-print s)))

    (define cry-asts (map stx-to-cry-ast stxs))

    (debug (printf "cry-ast:\n") (for ([ca cry-asts]) (pretty-print-ast ca) (newline)))

    (define res-stxs
      (parameterize ([current-compiler cmplr])
        (compile-defs cry-asts)))
    (debug (printf "result:\n~a\n" res-stxs))

    (strip-context
     #`(module default racket
         ;; #,@result-stxs
         ))))

;; maps to functions that take a list of stxs and return the final syntax for a file
(define compiler-maps (make-hash `((test . ,(create-test-top test-compiler)))))

(define (get-compiler key) (hash-ref compiler-maps key))
