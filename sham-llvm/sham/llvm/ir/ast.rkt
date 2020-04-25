#lang racket

(require sham/astg/ast)

(provide (all-defined-out))

(define-ast llvm
  #:custom-write #t
  (def
    [module (defs:def ...)]
    [function      (type blocks:ast.block ...)]
    [type          (ast:type)]
    [global        (type value)]
    [global-string (str)]
    [external      (type)]
    [intrinsic     (str type)]
    #:common-mutable info
    #:common id)
  (ast [block [name instructions:instruction ... term:terminator]]
       #:common-auto-mutable metadata)
  (type ast
        [internal ()]
        [ref      (to)]
        [struct   (fields:type ...)]
        [function (args:type ... var-arg? ret:type)]
        [pointer  (to:type)]
        [array    (of:type size)]
        [vector   (of:type size)])
  (instruction ast
               [op (result op flags args ...)])
  (terminator instruction
              [ret    (value)]
              [retv   ()]
              [br     (condition iftrue iffalse)]
              [bru    (destinition)]
              [switch (condition default (value dest) ...)])
  (value ast
         [fl     ((? fixnum?) type:type)]
         [si     ((? exact-integer?) type:type)]
         [ui     ((? exact-nonnegative-integer?) type:type)]
         [string ((? string?))]
         [llvm   (ref)]
         [basic-struct (fields)]
         [named-struct (fields type)]
         [array  (value type:type)]
         [vector (value ...)]
         [sizeof (type)]))
