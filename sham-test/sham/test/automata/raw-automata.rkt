#lang racket

(require sham
         (prefix-in rkt- sham/rkt))
(require (for-syntax syntax/parse))

(define-current-sham-env fsa-module #:late-pass `[always-inliner])
(define fmd (sham-function-metadata #:attributes `[always-inline]))


(define-sham-efunction
  (M-cadr (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
  (init inp pos len))

(define-sham-efunction #:md fmd
  (init (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
  (e-if^ rkt-bool (icmp-ult^ pos len)
         (rkt-e-sym-case rkt-bool (array-ref^ inp pos)
                       [(c) (more inp (add^ pos (ui64 1)) len)]
                       [else rkt-false])
         rkt-false))

(define-sham-efunction #:md fmd
  (more (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
  (e-if^ rkt-bool (icmp-ult^ pos len)
         (rkt-e-sym-case rkt-bool (array-ref^ inp pos)
                       [(a) (more inp (add^ pos (ui64 1)) len)]
                       [(d) (more inp (add^ pos (ui64 1)) len)]
                       [(r) (end inp (add^ pos (ui64 1)) len)]
                       [else rkt-false])
   rkt-false))

(define-sham-efunction #:md fmd
  (end (inp : rkt-sym*) (pos : i64) (len : i64) : rkt-bool)
  (e-if^ rkt-bool (icmp-ult^ pos len)
         (rkt-e-sym-case rkt-bool (array-ref^ inp pos)
                       [else rkt-false])
         rkt-true))

(module+ test
  (define jenv
    (parameterize ([sham-compile-options `(dump-sham
                                           dump-llvm
                                           dump-llvm-ir
                                           verify-with-error)])
      (sham-jit-compile! fsa-module #:opt-level 3)))
  (define init (jit-lookup-function jenv 'init)))
