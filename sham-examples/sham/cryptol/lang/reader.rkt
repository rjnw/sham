#lang racket

(require syntax/strip-context
         sham/sam/pretty)

(require "stx-to-cry-ast.rkt")
(provide (rename-out [cry-read read]
                     [cry-read-syntax read-syntax]))

(define (cry-read in)
  (syntax->datum (cry-read-syntax #f in)))

(define (cry-read-syntax src in)
  (printf "read-sytnax: ~a ~a\n" src in)
  (define res
    (parameterize (
                   ;; [read-square-bracket-as-paren #t]
                   ;; [read-square-bracket-with-tag #t]
                   ;; [read-curly-brace-with-tag #t]
                   [read-accept-bar-quote #f]
                   [read-accept-quasiquote #f]
                   )
      (let loop []
        (define stx (read-syntax src in))
        ;; (printf "read-syntax: ~a\n" stx)
        ;; (printf "syntax-property: ~a\n" (syntax-property stx 'paren-shape))
        (if (eof-object? stx) '() (cons stx (loop))))))
  (pretty-print-columns 160)
  (for ([s res])
    (println "read-syntax:")
    (pretty-print (syntax->datum s))
    (println "parsed-cry:")
    (pretty-print (pretty-print-ast (stx-to-cry-ast s))))

  (strip-context
   #`(module default racket
       42)))
