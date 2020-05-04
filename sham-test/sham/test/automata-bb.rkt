#lang racket

(require sham
         (prefix-in rkt- sham/rkt))

(define (raw-automata input input-length)
  (define-current-sham-env raw-module)

  (define-sham-function
    (M-cadr (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
    (label^ init
            (if^ (icmp-ult^ pos len)
                 (rkt-sym-case (array-ref^ inp pos)
                               [(c) (set!^ pos (add^ pos (ui64 1)))
                                    (label-jump^ more)]
                               [else (return^ rkt-false)])
                 (return^ rkt-false)))
    (label^ more
            (if^ (icmp-ult^ pos len)
                 (rkt-sym-case (array-ref^ inp pos)
                               [(a) (set!^ pos (add^ pos (ui64 1)))
                                    (label-jump^ more)]
                               [(d) (set!^ pos (add^ pos (ui64 1)))
                                    (label-jump^ more)]
                               [(r) (set!^ pos (add^ pos (ui64 1)))
                                    (label-jump^ end)]
                               [else (return^ rkt-false)])
                 (return^ rkt-false)))
    (label^ end
            (if^ (icmp-ult^ pos len)
                 (return^ rkt-false)
                 (return^ rkt-true))))

  (parameterize ([sham-compile-options `(dump-sham dump-llvm dump-llvm-ir verify-with-error)])
    (sham-jit-compile! raw-module #:opt-level 3))
  (M-cadr input 0 input-length))

(module+ test
  (require racket/random)
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
    (time (raw-automata sham-input input-length)))
  (run 10000))
