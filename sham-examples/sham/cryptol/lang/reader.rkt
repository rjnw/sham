#lang racket

(require syntax/strip-context
         sham/sam/pretty)

(require "stx-to-cry-ast.rkt"
         "../compiler/to-rkt-stx.rkt")
(provide (rename-out [cry-read read]
                     [cry-read-syntax read-syntax]))

(define (cry-read in)
  (syntax->datum (cry-read-syntax #f in)))

(define (cry-read-syntax src in)
  (printf "read-sytnax: ~a ~a\n" src in)
  (define (do-parse stx)
    (println "read-syntax:") (pretty-print (syntax->datum stx))
    (define cry-ast (stx-to-cry-ast stx))
    (println "parsed-cry:") (pretty-print-ast cry-ast) (newline)
    (define rkt-stx (cry-ast-to-rkt-stx cry-ast))
    (pretty-print (syntax->datum rkt-stx))
    rkt-stx)
  (define cdefs
    (for/list ([ln in])
      (parameterize ([read-accept-bar-quote #f]
                     [read-accept-quasiquote #f])
        (define stx (read-syntax ln in))
        (if (eof-object? stx) #`0 (do-parse stx)))))
  (strip-context
   #`(module default racket
       #,@cdefs)))
