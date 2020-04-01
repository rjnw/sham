#lang racket

(provide (all-defined-out))

(define (empty-assoc-env) '())
(define (assoc-env-lookup env key)
  (cond
    [(assoc key env) => cdr]
    [else (error "key not found in env" key)]))
(define (assoc-env-ref env key)
  (cond
    [(assoc key env) => cdr]
    [else #f]))

(define (assoc-env-contains? env key)
  (not (false? (assoc key env))))
(define (assoc-env-extend env key value)
  (cons (cons key value) env))


(struct llvm-module-env [module-ref context-ref ast value-refs])
(define (env-lookup-value-ref env key)
  (hash-ref (llvm-module-env-value-refs env) key))

(define (env-contains-value-ref? env key)
  (hash-has-key? (llvm-module-env-value-refs env) key))

(define (env-add-value-ref! env key value)
  (hash-set! (llvm-module-env-value-refs env) key value))
