#lang racket

(provide (all-defined-out))

(define (to-string s)
  (match s
    [(? symbol?) (symbol->string s)]
    [(? string?) s]
    [(? false?) s]
    [else (error 'sham "invalid name for llvm values ~a" s)]))
(define (assoc-env? v)
  (and (list? v)
       (andmap cons? v)))
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
