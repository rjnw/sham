#lang racket

(require "env.rkt")

(provide (all-defined-out))

(define info-sym '#%jit-info)
(define module-key 'module)
(define ffi-mapping-key 'ffi-mappings)
(define ffi-lib-key 'ffi-libs)
(define rkt-mapping-key 'rkt-mappings)

(define (build-on-info cinfo assocs)
  (define new-info (make-hash assocs))
  (when (hash? cinfo)
    (for ([(key value) (in-hash cinfo)])
      (unless (hash-has-key? new-info key)
        (hash-set! new-info key value))))
  new-info)

(define get-info-key)
(define (env-get-info-key sym mod-env)
  (define info (get-info mod-env))
  (hash-ref info sym (void)))

(define (env-add-info-key! key val mod-env)
  (define info (get-info mod-env))
  (hash-set! info key val))

(define (env-add-info mod-env info)
  (env-extend info-sym info mod-env))
(define (env-get-info mod-env)
  (env-lookup info-sym mod-env))
(define env-get-module (curry get-info-key module-key))
