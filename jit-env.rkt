#lang racket

(provide (all-defined-out))

(define (env-lookup key env)
  ;; (printf "env-lookup, env: ~a, key: ~a\n" env key)
  (define val (assoc key env))
  (if val
      (cdr val)
      (error "key not found in env ~a" key)))

(define (empty-env)
  '())

(define (env-extend key value env)
  (cons (cons key value) env))

(struct env-type (skel prim) #:prefab)
(struct env-jit-function (ref type) #:prefab)

(struct env-jit-internal-function (compiler))
(struct env-jit-value (ref type))
(struct env-c-function (type pointer))
(struct env-racket-function (type object))
(struct env-racket-ffi-function (type object))
