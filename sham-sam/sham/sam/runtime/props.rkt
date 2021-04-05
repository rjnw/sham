#lang racket

(provide (all-defined-out))
(define-values (prop:ast-constructor has-ast-constructor? get-ast-constructor)
  (make-struct-type-property 'sham:sam:ast-constructor))
