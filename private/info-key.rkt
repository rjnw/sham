#lang racket

(require "env.rkt")

(provide (all-defined-out))

(define (build-info cinfo assocs)
  (define new-info (make-hash assocs))
  (when (hash? cinfo)
    (for ([(key value) (in-hash cinfo)])
      (unless (hash-has-key? new-info key)
        (hash-set! new-info key value))))
  new-info)

(define (get-info-key sym mod-env)
  (define info  (jit-get-info mod-env))
  (hash-ref info sym (void)))

(define (add-info-key! key val mod-env)
  (define info (jit-get-info mod-env))
  (hash-set! info key val))

(define (add-info mod-env info)
  (env-extend '#%jit-info info mod-env))
(define (get-info mod-env)
  (env-lookup '#%jit-info mod-env))

(define (get-module mod-env)
  (jit-get-info-key 'jit-module mod-env))
