#lang racket

(require sham/astg/ast)

(define-ast llvm
  #:custom-write #t
  (def
    [module ('define-llvm-module name defs ...)
        [defs : def]
        #:define name]
    [function ('define-llvm-function name type blocks ...)
              [blocks : (ast block)]]
    [type (ast)]
    [global (type value)]
    [global-string (str)]
    [external (type)]
    [intrinsic (str type)]
    #:common-mutable metadata
    #:common id)
  (ast [block [name instructions:instruction ... term:terminator]]
       #:common-auto-mutable metadata)
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

(define (llvm-metadata v)
  (cond [(llvm:def? v) (llvm:def-metadata v)]
        [(llvm:ast? v) (llvm:ast-metadata v)]))
(define llvm-md llvm-metadata)
(define (llvm-metadata! v md)
  (cond [(llvm:def? v) (set-llvm:def-metadata! v md)]
        [(llvm:ast? v) (set-llvm:ast-metadata! v md)])
  v)
(define llvm-md! llvm-metadata!)
