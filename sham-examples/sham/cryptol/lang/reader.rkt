#lang racket

(require "../compiler/all.rkt")

(provide (rename-out [cry-read read]
                     [cry-read-syntax read-syntax]))

(define (cry-read in)
  (error 'sham/cryptol "cry-read")
  ;; (syntax->datum (cry-read-syntax #f in))
  )

(define (cry-read-syntax src in)
  (define-values (cw-key compiler-type cry-stxs)
    (values (read-syntax src in)
            (read-syntax src in)
            (let loop ()
              (parameterize ([read-accept-bar-quote #f]
                             [read-accept-quasiquote #f])
                (define rd (read-syntax src in))
                (if (eof-object? rd) '()
                    (cons rd (loop)))))))
  (define compiler
    (if (equal? (syntax->datum cw-key) '#:compile-with)
        (get-compiler (syntax->datum compiler-type))
        (error 'cryptol "unknown compiler ~a" (syntax->datum cw-key))))
  (compiler cry-stxs))
