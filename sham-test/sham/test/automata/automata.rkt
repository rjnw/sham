#lang racket

(require sham
         (prefix-in rkt- sham/rkt))

(require (for-syntax syntax/parse))
(require racket/fixnum
         racket/unsafe/ops)

(define bench-times (make-hash))
(struct bench-time [cpu real garbage] #:prefab)

(define print-time (make-parameter #f))

(define-syntax (benchmark stx)
  (syntax-parse stx
    [(_ mesg:expr (proc:id args:expr ...))
     #`(let-values ([(result cpu-ms real-ms garbage-ms) (time-apply proc (list args ...))])
         (when (print-time)
           (printf "sham:benchmark:~a: cpu: ~a, real: ~a, gc: ~a\n" mesg cpu-ms real-ms garbage-ms))
         (hash-set! bench-times mesg
                    (cons (bench-time cpu-ms real-ms garbage-ms) (hash-ref bench-times mesg '())))
         (apply values result))]))

(define (racket-automata input input-length)
  (define-syntax (define-fsa stx)
    (syntax-parse stx
      [(_ name start (end ...) [state ([input next-state] ...)] ...)
       #:with (res ...)
       (map (lambda (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'true #'false))
            (syntax->list #'(state ...)))
       #'(begin
           (define (name inp pos len)
             (start inp pos len))
           (define (state inp pos len)
             (if (< pos len)
                 (case (unsafe-vector-ref inp pos)
                   [(input) (next-state inp (+ pos 1) len)] ...
                   [else false])
                 res)) ...)]))
  (define-fsa M-simple
    s1 (s1)
    [s1 ([c s2]
         [a s2]
         [d s1])]
    [s2 ([c s1]
         [a s2]
         [r s2])])
  (define-fsa M-cadr
    init (end)
    [init ([c more])]
    [more ([a more]
           [d more]
           [r end])]
    [end ()])
  (values (benchmark (format "rkt-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
          (benchmark (format "rkt-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length))))

(define (sham-automata input input-length)
  (define-current-sham-env fsa-module #:late-pass `[always-inliner])
  (define fmd (sham-function-metadata #:attributes `[always-inline]))

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

  (define-fsa M-simple
    s1 (s1)
    [s1 ([c s2]
         [a s2]
         [d s1])]
    [s2 ([c s1]
         [a s2]
         [r s2])])

  (define-fsa M-cadr
    init (end)
    [init ([c more])]
    [more ([a more]
           [d more]
           [r end])]
    [end ()])

  (sham-jit-compile! fsa-module #:opt-level 3)
  (values (benchmark (format "sham-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
          (benchmark (format "sham-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length))))

(define (basic-block-automata input input-length)
  (define-current-sham-env fsa-module)

  (define-syntax (define-fsa stx)
    (syntax-parse stx
      [(_ name start (end ...) [state ([input next-state] ...)] ...)
       #:with (res ...)
       (map (lambda (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'rkt-true #'rkt-false))
            (syntax->list #'(state ...)))
       #`(define-sham-function
           (name (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
           (label^ state
                   (if^ (icmp-ult^ pos len)
                        (rkt-sym-case (array-ref^ inp pos)
                                      [(input) (set!^ pos (add^ pos (ui64 1)))
                                               (label-jump^ next-state)] ...
                                      [else (return^ rkt-false)])
                        (return^ res))) ...)]))

  (define-fsa M-simple
    s1 (s1)
    [s1 ([c s2]
         [a s2]
         [d s1])]
    [s2 ([c s1]
         [a s2]
         [r s2])])

  (define-fsa M-cadr
    init (end)
    [init ([c more])]
    [more ([a more]
           [d more]
           [r end])]
    [end ()])

  (sham-jit-compile! fsa-module #:opt-level 3)
  (values (benchmark (format "bb-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
          (benchmark (format "bb-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length))))


(module+ test
  (require rackunit
           ffi/unsafe
           racket/random)

  (define (run-test! (repeat-count 1) (input-length 100000))
    (define input (build-vector input-length (λ (i)
                                               (cond [(zero? i) 'c]
                                                     [(equal? i (sub1 input-length)) 'r]
                                                     [else (random-ref `(a d))]))))
    (define sham-input (benchmark (format "sham-rkt-vector-cast:~a" input-length)
                                  (rkt-vector rkt-sym input)))
    (for/list ([i repeat-count])
      (define info (format ":test ~a" i))
      (define-values (r-simple r-cadr) (racket-automata input input-length))
      (define-values (s-simple s-cadr) (sham-automata sham-input input-length))
      (define-values (b-simple b-cadr) (basic-block-automata sham-input input-length))
      (test-equal? (format "automata~a:~a sham:simple" info input-length) s-simple r-simple)
      (test-equal? (format "automata~a:~a sham:cadr" info input-length) s-cadr r-cadr)
      (test-equal? (format "automata~a:~a bb:simple" info input-length) b-simple r-simple)
      (test-equal? (format "automata~a:~a bb:cadr" info input-length) b-cadr r-cadr)
      `(,i (rkt ,r-simple ,r-cadr)
           (sham ,s-simple ,s-cadr)
           (basic-block ,b-simple ,b-cadr))))

  (parameterize ([sham-compile-options `(;; dump-sham dump-llvm dump-llvm-ir-after-opt verify-with-error
                                         )])
    (pretty-print (run-test! 4)))
  (pretty-print bench-times))
