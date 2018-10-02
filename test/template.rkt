#lang racket

(require "../main.rkt")


(module+ test
  (require rackunit)

  (define mod
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      ...)))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)
  (define ... (jit-get-function '... mod-env))
  (check-eq? ... ...))
