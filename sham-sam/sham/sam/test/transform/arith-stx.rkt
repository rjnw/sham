#lang racket

(require syntax/parse)

(require sham/sam/transform)

(require "../ast/basic-math.rkt")

#;(define-ast math
  (expr
   [neg ('- e)]
   [div ('/ n d)]
   [add ('+ e ...)]
   [sub ('- e1 e2 ...)]
   [mul ('* e ...)])
  #:with struct-helpers sexp-printer
  #:format (#f - #f - -))

(define-transform (parse-math)
  (rkt-syntax -> math-ast)
  (mexpr (stx -> val)
         [n:integer (make-num (syntax-e n))]
         [('- e:mexpr e2:mexpr ...) (make-sub e e2)]
         [('/ n:mexpr d:mexpr) (make-div n d)]
         [('+ es:mexpr ...) (make-add es)]
         [('* es:mexpr ...) (make-mul es)]))

(module+ test
  (require rackunit)

  )
