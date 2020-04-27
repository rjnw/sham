#lang racket

(require sham/ir/ast/core
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple))

(require (for-syntax racket/syntax
                     syntax/parse)
         syntax/parse/define)

(provide (except-out (all-defined-out)
                     define-alias))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (to-prefix:id from-prefix:id)
        [names:id ...])
     #:with (to-names ...) (map (λ (n) (format-id n "~a~a" #'to-prefix n)) (syntax->list #`(names ...)))
     #:with (from-names ...) (map (λ (n) (format-id n "~a~a" #'from-prefix n)) (syntax->list #`(names ...)))
     #:with (to-check-names ...) (map (λ (n) (format-id n "~a~a?" #'to-prefix n)) (syntax->list #`(names ...)))
     #:with (from-check-names ...) (map (λ (n) (format-id n "~a~a?" #'from-prefix n)) (syntax->list #`(names ...)))
     #`(begin (define to-names from-names) ...
              (define to-check-names from-check-names) ...)]))

(define-alias (d- sham:def:)
  [module function struct racket])

(define-alias (t- llvm-type-)
  [ref pointer array vector struct])

(define (t-function args ret #:var-arg (var-arg? #f)) (llvm-type-function args var-arg? ret))
(define t-function? llvm:ast:type:function?)

(define-alias (s- sham:ast:stmt:)
  [set! if switch continue break while void expr block label return return-void])

(define-alias (e- sham:ast:expr:)
  [ref op access void etype let])

(define-alias (r- sham:ast:rator:)
  [reference llvm intrinsic external])

;; metadata
(define metadata sham:ast-metadata)
(define metadata! set-sham:ast-metadata!)

;; checks
(define (type? v)
  (or (llvm:ast:type? v) (sham:ast:expr:etype? v)))
(define (def? v) (or (llvm:def? v) (sham:def? v)))
(define ast? sham:ast?)
(define stmt? sham:ast:stmt?)
(define expr? (or/c sham:ast:expr? llvm:ast:value?))
(define rator? sham:ast:rator?)
