#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime)

(provide (all-defined-out))

(define-ast llvm
  (def
    [module (defs:def ...)]
    [function      (type blocks:ast.block ...)]
    [block         (instructions:instruction ... term:terminator)]
    [type          (ast:type)]
    [global        (type value)]
    [global-string (str)]
    [external      (type)]
    [intrinsic     (str type)]
    #:common id)
  (type
   [ref      (to)]
   [struct   (fields:type ...)]
   [function (args:type ... var-arg? ret:type)]
   [pointer  (to:type)]
   [array    (of:type size)]
   [vector   (of:type size)])
  (instruction
   [op (result op flags args ...)])
  (terminator instruction
              [ret    (value)]
              [retv   ()]
              [br     (condition iftrue iffalse)]
              [bru    (destinition)]
              [switch (condition default (value dest) ...)])
  (value
   [param  ((? val exact-nonnegative-integer?))]
   [ref    (to)]
   [fl     ((? val fixnum?) type:type)]
   [si     ((? val exact-integer?) type:type)]
   [ui     ((? val exact-nonnegative-integer?) type:type)]
   [string ((? val string?))]
   [llvm   (ref)]
   [basic-struct (fields)]
   [named-struct (fields type)]
   [array  (value type:type)]
   [vector (value ...)]
   [sizeof (type)])
  #:with struct-helpers)

(define (llvm-metadata v) (ast:metadata-custom (ast-md v)))
(define llvm-md llvm-metadata)

(define (set-llvm-metadata! v md)
  (set-ast:metadata-custom! (ast-md v) md))
(define set-llvm-md! set-llvm-metadata!)
