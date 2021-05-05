#lang racket

(require sham/llvm/ir/ast
         sham/sam/alias)

(provide (all-defined-out))

(define-aliases llvm
  [def]
  [value -> val]
  [type]
  [instruction -> inst]
  [terminator -> inst])
