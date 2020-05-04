#lang racket

(provide (all-defined-out))

(define (to-string s)
  (match s
    [(? symbol?) (symbol->string s)]
    [(? string?) s]
    [(? false?) s]
    [else (error 'sham "invalid name for sham values ~a" s)]))

(define normalize-id to-string)

(define (assoc-env? v)
  (and (list? v)
       (andmap cons? v)))
(define (empty-assoc-env) '())
(define (assoc-env-lookup env key)
  (cond
    [(assoc key env) => cdr]
    [else (error 'sham:assoc "key not found in association env" key)]))
(define (assoc-env-ref env key)
  (cond
    [(assoc key env) => cdr]
    [else #f]))

(define (assoc-env-contains? env key)
  (not (false? (assoc key env))))
(define (assoc-env-extend env key value)
  (cons (cons key value) env))

(define (assoc-default assc keys (defaultf #f))
  (cond
    [(not (list? keys)) (assoc-default assc (list keys) defaultf)]
    [(for/first [(a assc)
                 #:when (member (car a) keys)]
       (cdr a))]
    [(procedure? defaultf) (defaultf)]
    [defaultf]
    [else (error 'sham:assoc "error looking up association list ~a,~a" assc keys)]))
