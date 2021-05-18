#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         sham/md)

(provide (all-defined-out))

(define-ast llvm
  (def
    [module (defs:def ...) #:default-metadata (empty-module-md)]
    [function      (type blocks:def.block ...) #:default-metadata (empty-function-md)]
    [block         (instructions:instruction ... term:terminator)]
    [type          (ast:type)]
    [global        (type value)]
    [global-string (str)]
    [external      (type)]
    [intrinsic     (str type)]
    #:common id:id)
  (type
   [ref      (to)]
   [struct   (fields:type ...)]
   [function (args:type ... var-arg? ret:type)]
   [pointer  (to:type)]
   [array    (of:type size)]
   [vector   (of:type size)]
   #:default-metadata (empty-type-md))
  (instruction
   [op (result op flags (args ...))]
   #:default-metadata (empty-instruction-md))
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
   [basic-struct (fields ...)]
   [named-struct (fields ... type)]
   [array  (value type:type)]
   [vector (value ...)]
   [sizeof (type)])
  #:with struct-helpers
  #:default-metadata (empty-md))

(define (llvm-metadata v)
  (match v
    [(? ast?) (llvm-metadata (ast:metadata-custom (ast-md v)))]
    [(? ast:metadata?) (llvm-metadata (ast:metadata-custom v))]
    [(? metadata?) v]
    [(list mds ...) (findf metadata? mds)]
    [else (error 'sham/llvm "unknown value for getting metadata: ~a" v)]))

(define llvm-md llvm-metadata)
