#lang racket

(provide (all-defined-out))

(define (env-lookup key env)
  ;; (printf "env-lookup, env: ~a, key: ~a\n" env key)
  (define val (assoc key env))
  (if val
      (cdr val)
      (error "key not found in env" key)))

(define (env-contains? key env)
  (not (false? (assoc key env))))

(define (empty-env)
  '())

(define (env-extend key value env)
  (cons (cons key value) env))

(struct env-type (skel prim) #:prefab)
(struct env-function (ref type) #:prefab)
(struct env-internal-function (builder) #:prefab)

(struct env-value (ref type) #:prefab)
(struct env-racket-function (type object) #:prefab)
(struct env-racket-ffi-function (type object) #:prefab)