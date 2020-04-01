#lang racket

(require sham/astg/ast)

(provide (all-defined-out))

(define-ast llvm
  #:custom-write #t
  (def
    [module (defs:def ...)]
    [function      ((arg-ids:terminal.sym arg-types:type) ... ret-type:type blocks:ast.block ...)]
    [type          (type:type)]
    [global        (type:type value:constant)]
    [global-string (str:terminal.string)]
    #:common-mutable info
    #:common id:terminal.sym)
  (ast [block [name:terminal.sym instructions:instruction ... term:terminator]]
       #:common-auto-mutable metadata)
  (type ast
        [internal ()]
        [ref      (to:terminal.sym)]
        [struct   (fields:type ...)]
        [function (args:type ... ret:type)]
        [pointer  (to:type)]
        [array    (of:type size:terminal.unsigned-int)]
        [vector   (of:type size:terminal.unsigned-int)])
  (instruction ast
               [op (result op flags args ...)])
  (terminator instruction
              [ret (value)]
              [retv ()]
              [br (condition iftrue iffalse)]
              [bru (destinition)]
              [switch (condition default (value dest) ...)])
  (constant ast
            [fl     (value:terminal.float        type:type)]
            [si     (value:terminal.signed-int   type:type)]
            [ui     (value:terminal.unsigned-int type:type)]
            [string (value:terminal.string)]
            [llvm   (value:terminal.llvm         type:type)]
            [struct (value:terminal.struct       type:type)]
            [array  (value:terminal.array        type:type)]
            [vector (value:terminal.vector)]))
