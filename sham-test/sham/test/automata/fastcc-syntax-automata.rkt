#lang racket

(require sham
         sham/llvm/ir/callconv
         (prefix-in rkt- sham/rkt))
(require (for-syntax syntax/parse))

(define-current-sham-env fsa-module)
(define fmd (sham-function-metadata #:calling-convention fastcc-md))

(define-syntax (define-fsa stx)
  (syntax-parse stx
    [(_ name start (end ...) [state ([input next-state] ...)] ...)
     #:with (res ...)
     (map (lambda (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'rkt-true #'rkt-false))
          (syntax->list #'(state ...)))
     #'(begin
         (define-sham-efunction (name (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
           (start inp pos len))
         (define-sham-efunction #:md fmd (state (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
           (e-if^ rkt-bool (icmp-ult^ pos len)
                  (rkt-e-sym-case rkt-bool (array-ref^ inp pos)
                                  [(input) (next-state inp (add^ pos (ui64 1)) len)] ...
                                  [else rkt-false])
                  res)) ...)]))

(define-fsa M-cadr
  init (end)
  [init ([c more])]
  [more ([a more]
         [d more]
         [r end])]
  [end ()])

(module+ test
  (require rackunit
           racket/random)
  (parameterize ([sham-compile-options `(dump-sham dump-llvm dump-llvm-ir verify-with-error)])
    (sham-jit-compile! fsa-module #:opt-level 3))

  (define (run (input-length 1000000))
    (define input
      (build-vector input-length
                    (λ (i)
                      (cond [(zero? i) 'c]
                            [(equal? i (sub1 input-length)) 'r]
                            [else (random-ref `(a d))]))))
    (printf "0\n")
    (define sham-input (rkt-vector rkt-sym input))
    (printf "1\n")
    (time (M-cadr sham-input 0 input-length))))
