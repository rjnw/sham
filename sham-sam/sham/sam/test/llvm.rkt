#lang racket

(require sham/sam/ast)

(define-ast llvm
  (def
    [module ('define-llvm-module name defs:def ...)
        #:define name]
    [function ('define-llvm-function name type blocks:ast.block ...)]
    [type (ast)]
    [global (type value)]
    [global-string (str)]
    [external (type)]
    [intrinsic (str type)]
    #:common id)
  (ast [block [name instructions:instruction ... term:terminator]])
  (type ast
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
         [param  ((? exact-nonnegative-integer?))]
         [ref    (to)]
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
