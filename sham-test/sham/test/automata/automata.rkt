#lang racket

(require sham
         sham/llvm/ir/callconv
         (prefix-in rkt- sham/rkt))

(require (for-syntax syntax/parse))
(require racket/fixnum
         racket/unsafe/ops)

(define bench-times (make-hash))
(struct bench-time [cpu real garbage] #:prefab)

(define print-time (make-parameter #f))

(define-syntax (benchmark stx)
  (syntax-parse stx
    [(_ repeat mesg:expr (proc:id args:expr ...))
     #`(for/last ([r repeat])
         (let-values ([(result cpu-ms real-ms garbage-ms) (time-apply proc (list args ...))])
           (when (print-time)
             (printf "sham:benchmark:~a: cpu: ~a, real: ~a, gc: ~a\n" mesg cpu-ms real-ms garbage-ms))
           (hash-set! bench-times mesg
                      (cons (bench-time cpu-ms real-ms garbage-ms) (hash-ref bench-times mesg '())))
           (apply values result)))]))

(define (average-times)
  (for/hash ([(k times) bench-times])
    (define-values (cpu real garbage)
      (for/fold [(cpu 0)
                 (real 0)
                 (garbage 0)]
                [(t times)]
        (match-define (bench-time c r g) t)
        (values (+ cpu c) (+ real r) (+ garbage g))))
    (define l (exact->inexact (length times)))
    (values k (bench-time (/ cpu l) (/ real l) (/ garbage l)))))

(define (racket-automata input input-length ir)
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
  #;(define-fsa M-simple
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
  #;(values (benchmark ir (format "rkt-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
          (benchmark ir (format "rkt-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))
  (benchmark ir (format "rkt-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))

(define (sham-automata input input-length ir)
  (define-current-sham-env default-module #:early-pass `[always-inliner])
  (define fmd (sham-function-metadata #:attributes `[always-inline]))

  (define-syntax (define-fsa stx)
    (syntax-parse stx
      [(_ name start (end ...) [state ([input next-state] ...)] ...)
       #:with (res ...)
       (map (lambda (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'rkt-true #'rkt-false))
            (syntax->list #'(state ...)))
       #'(begin
           (define-sham-function (name (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
             (return^ (start inp pos len)))
           (define-sham-function #:md fmd (state (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
             (if^ (icmp-ult^ pos len)
                  (switch^ (array-ref^ inp pos)
                           [(rkt-sym input) (return^ (next-state inp (add^ pos (ui64 1)) len))] ...
                           (return^ rkt-false))
                  (return^ res))) ...)]))

  #;(define-fsa M-simple
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

  (sham-jit-compile! default-module #:opt-level 3)
  #;(values (benchmark ir (format "sham-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
          (benchmark ir (format "sham-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))
  (benchmark ir (format "sham-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))

(define (fastcc-automata input input-length ir)
  (define-current-sham-env fastcc-module)
  (define fmd (sham-function-metadata #:calling-convention fastcc-md))

  (define-syntax (define-fsa stx)
    (syntax-parse stx
      [(_ name start (end ...) [state ([input next-state] ...)] ...)
       #:with (res ...)
       (map (lambda (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'rkt-true #'rkt-false))
            (syntax->list #'(state ...)))
       #'(begin
           (define-sham-function (name (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
             (return^ (start inp pos len)))
           (define-sham-function #:md fmd (state (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
             (if^ (icmp-ult^ pos len)
                  (switch^ (array-ref^ inp pos)
                           [(rkt-sym input) (return^ (next-state inp (add^ pos (ui64 1)) len))] ...
                           (return^ rkt-false))
                  (return^ res))) ...)]))

  #;(define-fsa M-simple
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

  (sham-jit-compile! fastcc-module #:opt-level 3)
  #;(values (benchmark ir (format "fastcc-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
            (benchmark ir (format "fastcc-fsa-cadr:~a" input-length)
                       (M-cadr input 0 input-length)))
  (benchmark ir (format "fastcc-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))


(define (basic-block-automata input input-length ir)
  (define-current-sham-env basic-block-module)

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
                        (switch^ (array-ref^ inp pos)
                                 [(rkt-sym input) (set!^ pos (add^ pos (ui64 1)))
                                          (label-jump^ next-state)] ...
                                 (return^ rkt-false))
                        (return^ res))) ...)]))

  #;(define-fsa M-simple
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

  (sham-jit-compile! basic-block-module #:opt-level 3)
  #;(values (benchmark ir (format "bb-fsa-simple:~a" input-length)
                     (M-simple input 0 input-length))
          (benchmark ir (format "bb-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))
  (benchmark ir (format "bb-fsa-cadr:~a" input-length)
                     (M-cadr input 0 input-length)))


(module+ test
  (require rackunit
           ffi/unsafe
           racket/random)

  (define (run-test! (repeat-count 1) (internal-repeat 1) (input-length 100000))
    (define input (build-vector input-length (λ (i)
                                               (cond [(zero? i) 'c]
                                                     [(equal? i (sub1 input-length)) 'r]
                                                     [else (random-ref `(a d))]))))
    (define sham-input (benchmark 1 (format "sham-rkt-vector-cast:~a" input-length)
                                  (rkt-array-from-vector rkt-sym input)))
    (printf "done-making-input\n")
    (for ([i repeat-count])
      (define info (format ":test ~a" i))
      (define r  (racket-automata input input-length internal-repeat))
      (define s (sham-automata sham-input input-length internal-repeat))
      (define b (basic-block-automata sham-input input-length internal-repeat))
      (define f (fastcc-automata sham-input input-length internal-repeat))
      (test-equal? (format "automata~a:~a sham:cadr" info input-length) s r)
      (test-equal? (format "automata~a:~a bb:cadr" info input-length) b r)
      (test-equal? (format "automata~a:~a fastcc:cadr" info input-length) f r)))

  (parameterize ([sham-compile-options `(;; dump-sham dump-llvm
                                         dump-llvm-ir-after-opt verify-with-error)])
    (run-test! 1 10 1000))
  (pretty-print (average-times)))
