#lang racket

(require sham/astg/ast)

(provide (all-defined-out))

(define-ast llvm
  #:custom-write #t
  (def
    [module (defs:def ...)]
    [function      ((arg-ids arg-types) ... ret-type blocks:ast.block ...)]
    [type          (type)]
    [global        (type value:constant)]
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
        [function (args:type ... ret:type)]
        [pointer  (to:type)]
        [array    (of:type size)]
        [vector   (of:type size)])
  (instruction ast
               [op (result op flags args ...)])
  (terminator instruction
              [ret (value)]
              [retv ()]
              [br (condition iftrue iffalse)]
              [bru (destinition)]
              [switch (condition default (value dest) ...)])
  (constant ast
            [fl     ((? fixnum?) type:type)]
            [si     ((? exact-integer?) type:type)]
            [ui     ((? exact-nonnegative-integer?) type:type)]
            [string ((? string?))]
            [llvm   (value type:type)]
            [basic-struct (fields)]
            [named-struct (fields type)]
            [array  (value type:type)]
            [vector (value ...)]))
