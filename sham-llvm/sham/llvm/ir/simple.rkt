#lang racket

(require sham/llvm/ir/ast
         sham/sam/alias)

(provide (all-defined-out))

(define-aliases llvm
  #:sep -
  [def]
  [value -> val]
  [type]
  [instruction -> inst]
  [terminator -> inst])
