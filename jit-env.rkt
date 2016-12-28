#lang racket

(provide (all-defined-out))

(define (env-lookup key env)
  ;; (pretty-display env)
  ;; (printf "env-lookup: ~a\n" key)
  (cdr (assoc key env)))
(define (empty-env)
  '())
(define (env-extend key value env)
  (cons (cons key value) env))



(struct env-type (skel prim) #:prefab)
(struct env-jit-function-decl (type object))
(struct env-jit-function (type ast object cpointer))
(struct env-jit-intrinsic-function (type ffi-pointer))
(struct env-c-function (type pointer))
(struct env-racket-function (type object))
