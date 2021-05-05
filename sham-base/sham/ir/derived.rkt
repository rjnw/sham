#lang racket

(require sham/ir/ast
         sham/ir/simple
         sham/llvm/ir/ast)

(require syntax/parse/define)

(provide (all-defined-out))

(define (broken? s)
  (or (sham:ast:stmt:break? s)
      (sham:ast:stmt:continue? s)
      (match s
        [(sham:ast:stmt:block _ (list _ ... end)) (broken? end)]
        [(sham:ast:stmt:label _ _ ss) (broken? ss)]
        [(? llvm:ast:instruction:terminator?) #t]
        [(? sham:ast:stmt?) #f]
        [else (error 'sham:ir:derived "unknown statement ~a" s)])))

(define (break-if-needed b) (if (broken? b) b (block^ b (s-break))))

(define (multi-switch test checkss bodys default)
  (define check-bodys
    (map (λ (cs b) (append (cdr (map (const (s-void)) cs))
                           (list (break-if-needed b))))
         checkss bodys))
  (s-switch test (flatten checkss) (flatten check-bodys) default))
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

(define (e-if^ type test thn els)
  (let^ ([if-result : type])
        (if^ test (set!^ if-result thn) (set!^ if-result els))
        if-result))

(define (array-ref^ array index) (load^ (gep^ array index)))
(define (array-set!^ array index value) (store!^ (gep^ array index) value))
