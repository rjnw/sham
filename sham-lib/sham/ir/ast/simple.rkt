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

(define-alias (v- llvm-val-)
  [param ref fl si ui string llvm basic-struct named-struct array vector])

(define (e-app rator flags rands)
  (match rator
    [(? sham:ast:rator?) (e-op rator flags rands)]
    [(sham:ast:expr:ref md v) (e-op (r-reference v) flags rands)]
    [(? symbol?) (e-op (r-reference rator) flags rands)]
    [(? string?)
     (if (type? (car rands))
         (e-op (r-intrinsic rator (car rands)) flags (cdr rands))
         (error 'sham:ir "expected type for intrinsic as first argument, ~a/~a" rator (car rands)))]
    [(? procedure?) (apply rator rands)]
    [else (error 'sham:ir "expected rator for app^ given: ~a" rator)]))

;; checks
(define (type? v)
  (or (llvm:ast:type? v) (sham:ast:expr:etype? v)))
(define (def? v) (or (llvm:def? v) (sham:def? v)))
(define ast? sham:ast?)
(define stmt? sham:ast:stmt?)
(define expr? (or/c sham:ast:expr? llvm:ast:value?))
(define rator? sham:ast:rator?)
