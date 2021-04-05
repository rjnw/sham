#lang racket

(require sham/llvm/ir/ast
         sham/sam/alias)

(provide (all-defined-out))

(define-alias #:for llvm
  #:sep -
  [def -> lld]
  [value -> llv]
  [type -> llt]
  [instruction -> lli]
  [terminator -> lli])
