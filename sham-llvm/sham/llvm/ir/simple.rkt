#lang racket

(require syntax/parse/define
         syntax/parse
         (for-syntax racket/syntax
                     racket/match))

(require sham/llvm/ir/ast)
(provide (all-defined-out))

(define def-module llvm:def:module)
(define def-function llvm:def:function)
(define def-type llvm:def:type)
(define def-global llvm:def:global)
(define def-global-string llvm:def:global-string)
(define def-external llvm:def:external)
(define def-intrinsic llvm:def:intrinsic)

(define ast-block llvm:ast:block)
(define ast-op llvm:ast:instruction:op)
(define ast-ret-void llvm:ast:instruction:terminator:retv)
(define ast-ret llvm:ast:instruction:terminator:ret)
(define ast-br llvm:ast:instruction:terminator:br)
(define ast-bru llvm:ast:instruction:terminator:bru)
(define ast-switch llvm:ast:instruction:terminator:switch)

(define const-fl llvm:ast:constant:fl)
(define const-si llvm:ast:constant:si)
(define const-ui llvm:ast:constant:ui)
(define const-string llvm:ast:constant:string)
(define const-llvm llvm:ast:constant:llvm)
(define const-basic-struct llvm:ast:constant:basic-struct)
(define const-named-struct llvm:ast:constant:named-struct)
(define const-array llvm:ast:constant:array)
(define const-vector llvm:ast:constant:vector)

(define type-internal llvm:ast:type:internal)
(define type-ref llvm:ast:type:ref)
(define type-struct llvm:ast:type:struct)
(define type-function llvm:ast:type:function)
(define type-pointer llvm:ast:type:pointer)
(define type-array llvm:ast:type:array)
(define type-vector llvm:ast:type:vector)

(define-syntax (define-ref-types stx)
  (syntax-parse stx
    [(_ names:id ...)
     (define name-list (map (λ (n) (map (λ (i) (format-id n "~a~a" n i))
                                        (build-list 4 (λ (j) (make-string j #\*)))))
                            (syntax->list #`(names ...))))
     (define (rec prev l)
       (match* (prev l)
         [(#f (cons curr next))
          (cons #`(define #,curr (type-ref (quote #,curr))) (rec curr next))]
         [(p (cons curr next))
          (cons #`(define #,curr (type-pointer #,p)) (rec curr next))]
         [(p empty) empty]))
     #`(begin #,@(apply append (map (λ (n) (rec #f n)) name-list)))]))

(define-ref-types i1 i8 i16 i32 i64 f32 f64 void)

(define-syntax (define-basic-ops stx)
  (syntax-parse stx
    [(_ op-name:id ...)
     #`(begin (define (op-name result args #:flags (flags #f)) (ast-op result (quote op-name) flags args)) ...)]))

(define-basic-ops
  icmp-eq icmp-ne icmp-ugt icmp-uge icmp-ult icmp-ule icmp-sgt icmp-sge icmp-slt icmp-sle
  fcmp-oeq fcmp-ogt fcmp-oge fcmp-olt fcmp-ole fcmp-one fcmp-ord fcmp-uno fcmp-ueq
  fcmp-ugt fcmp-uge fcmp-ult fcmp-ule fcmp-une
  add add-nsw add-nuw fadd
  sub sub-nsw sub-nuw fsub
  mul mul-nsw mul-nuw fmul
  udiv sdiv exact-sdiv fdiv
  urem srem frem
  shl lshr ashr
  or xor and
  extract-element insert-element
  trunc zext sext fp->ui fp->si ui->fp si->fp
  fp-trunc fp-ext ptr->int int->ptr
  bitcast addr-space-cast zext-or-bit-cast sext-or-bit-cast
  ptr-cast int-cast fp-cast
  neg neg-nsw neg-nuw fneg
  not load malloc alloca free store! array-malloc array-alloca
  gep phi)
