#lang racket

(require sham/ast/core
         sham/llvm/ir/ast
         (prefix-in llvm- sham/llvm/ir/simple))

(require (for-syntax racket/syntax syntax/parse))

(provide (all-defined-out))

(define-simple-macro (define-llvm-alias names:id ...)
  #:with (llvm-names ...) (map (λ (n) (format-id n "llvm-~a" n))
                         (syntax->list #`(names ...)))
  (begin (define names llvm-names) ...))

(define-llvm-alias
  def-module def-function def-type def-global def-global-string def-external def-intrinsic
  ast-block ast-op ast-ret-void ast-ret ast-br ast-bru ast-switch
  val-ref val-param val-fl val-si val-ui val-string val-llvm val-basic-struct val-named-struct val-array val-vector
  type-internal type-ref type-struct type-function type-pointer type-array type-vector)

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (to-prefix:id from-prefix:id)
        [names:id ...])
     #:with (to-names ...) (map (λ (n) (format-id n "~a~a" #'to-prefix n)) (syntax->list #`(names ...)))
     #:with (from-names ...) (map (λ (n) (format-id n "~a~a" #'from-prefix n)) (syntax->list #`(names ...)))
     #:with (to-check-names ...) (map (λ (n) (format-id n "~a~a?" #'to-prefix n)) (syntax->list #`(names ...)))
     #:with (from-check-names ...) (map (λ (n) (format-id n "~a~a?" #'from-prefix n)) (syntax->list #`(names ...)))
     #`(begin (define to-names from-names) ...
              (define to-check-name from-check-name) ...)]))

(define-alias (d- sham:def:)
  [module function struct racket])

(define-alias (t- llvm-type-)
  [ref ptr array vector])

(define t-llvm-struct llvm-type-struct)
(define t-llvm-struct? llvm:ast:type:struct?)
(define t-struct sham:ast:type:struct)
(define t-struct? sham:ast:type:struct?)
(define (t-fun args ret #:var-arg (var-arg? #f)) (llvm-type-fun args var-arg? ret))
(define t-fun? llvm:ast:type:function?)

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
(define expr? (or/c sham:ast:expr? llvm:ast:constant?))
(define rator? sham:ast:rator?)
