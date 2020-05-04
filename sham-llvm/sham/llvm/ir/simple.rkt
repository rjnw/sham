#lang racket

(require (for-syntax racket/syntax
                     syntax/parse))

(require sham/llvm/ir/ast)
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

(define-alias (def- llvm:def:)
  [module function type global global-string external intrinsic])
(define-alias (ast- llvm:ast:)
  [block])
(define-alias (ast- llvm:ast:instruction:)
  [op])
(define-alias (ast- llvm:ast:instruction:terminator:)
  [retv ret br bru switch])
(define-alias (val- llvm:ast:value:)
  [param ref fl si ui string llvm basic-struct named-struct array vector])
(define-alias (type- llvm:ast:type:)
  [ref struct function pointer array vector])
