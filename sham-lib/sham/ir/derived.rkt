#lang racket

(require sham/ir/ast/simple
         sham/ir/ast/syntax
         sham/ir/ast/op)

(require syntax/parse/define)

(provide (all-defined-out))

(define (multi-switch test checkss bodys default)
  (define check-bodys
    (map (λ (cs b) (append (cdr (map (const (s-void)) cs)) (list (block^ b (s-break)))))
         checkss bodys))
  (s-switch test (flatten checkss) check-bodys default))
(define m-switch multi-switch)

(define-simple-macro (m-switch^ test [(checks ...) body] ... default)
  (m-switch test (list (list checks ...) ...) (list body ...) default))

(define (e-m-switch type test checkss bodys default)
  (let^ ([switch-result : type])
        (multi-switch test checkss (map (curry s-set! switch-result) bodys) default)))

(define-simple-macro (e-m-switch^ type test [(checks ...) body] ... default)
  (let^ ([switch-result : type])
        (m-switch^ test
                   [(checks ...) (set!^ switch-result body)] ...
                   (set!^ switch-result default))
        switch-result))

(define (array-ref array index) (load^ (gep^ array index)))
(define (array-set! array index value) (store!^ (gep^ array index) value))
