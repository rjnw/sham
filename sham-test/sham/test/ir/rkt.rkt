#lang racket

(require sham
         (prefix-in rkt- sham/rkt))

(define-current-sham-env test-env)

(define-sham-efunction
  (test-sym-case (inp : rkt-sym) : rkt-bool)
  (rkt-e-sym-case
   rkt-bool inp
   [(k) rkt-true]
   [else rkt-false]))

(define-sham-efunction (test-sym-identity (i : rkt-sym) : rkt-sym) i)

(define-sham-efunction
  (test-array-ref (v : rkt-sym*) (i : i64) : rkt-sym)
  (array-ref^ v i))

(define-sham-efunction
  (test-array-ref-int (v : i64*) (i : i64) : i64)
  (array-ref^ v i))

(module+ test
  (require rackunit)
  (define test-jit-env
    (parameterize ([sham-compile-options `(dump-sham dump-llvm dump-llvm-ir verify-with-error)])
      (sham-jit-compile! test-env #:opt-level 3)))
  (test-true "sham:rkt:sym:true" (test-sym-case 'k))
  (test-false "sham:rkt:sym:false" (test-sym-case 'd))
  (test-equal? "sham:rkt:sym:identity" (test-sym-identity '42) '42)
  (pretty-print (list (test-array-ref (rkt-list rkt-sym '(a b c d)) 0)
                      (test-array-ref-int (rkt-list i64 '(0 1 2 3 4 5)) 0)))

  (define (runner (start 100000) (end 100000) (step 100))
    (define input (build-vector end identity))
    (define sham-input (rkt-vector rkt-sym input))
    (for ([i (in-range start end step)])
      ;; (printf "~a\n" i)
      (test-array-ref sham-input i))))
