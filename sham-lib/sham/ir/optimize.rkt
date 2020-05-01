#lang racket

(require sham/ir/env
         sham/llvm/ir/optimize)

(provide sham-env-optimize!)

(define (sham-env-optimize! s-env
                            #:opt-level [olevel 1]
                            #:size-level [slevel 1]
                            #:loop-vec [lvec #f]
                            #:slp-vec [svec #f])
  (optimize-llvm-module! (sham-env-ll-env s-env)
                         #:opt-level olevel
                         #:size-level slevel
                         #:loop-vec lvec
                         #:slp-vec svec)
  s-env)
