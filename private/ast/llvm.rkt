#lang racket

(provide (all-defined-out))

(define-struct llvm:module (name layout target passes defns))

(define-struct llvm:defn:function
  (name arg-syms arg-types ret-type attrs passes stmt))
(define-struct llvm:defn:type (name type))

(define-struct llvm:type:label (sym))
(define-struct llvm:type:struct (fields))
(define-struct llvm:type:function (args ret))
(define-struct llvm:type:pointer (to))

(define-struct llvm:stmt:block (label stmts))
(define-struct llvm:stmt:set! (sym expr))
(define-struct llvm:stmt:store! (expr-t expr-v))
(define-struct llvm:stmt:cond-branch (expr label-t label-e))
(define-struct llvm:stmt:return (expr))
(define-struct llvm:stmt:return-void ())

(define-struct llvm:expr:type (sym))
(define-struct llvm:expr:calli (sym exprs))
(define-struct llvm:expr:call (sym exprs))
(define-struct llvm:expr:load (expr))
(define-struct llvm:expr:value (v type))
(define-struct llvm:expr:gep (expr exprs))
(define-struct llvm:expr:phi (exprs labels))

#|
;;statements
('block label
 (stmts ...))

(set! symbol expression)
(store! exp exp)

(cond-branch exp label label)
(branch label)

(return exp)
(return-void)

;;expressions
(type symbol)
(calli intr-sym exp ...)
(call sym exp ...)
(load exp)
(value v type)
(gep exp (exp ...))
(phi ((exp label) ...))

;;types
label ;previously defined type
<internal-types> ;i1 i8 i16 ... i128 f32  f64 void
(struct type ...)
(pointer type)

;;module-stmt
('function (name ((sym type) ...) type)
           (attrs ...) 
           stmt)
('type name type)

;;top
('module name 
    data-layout
    target
    module-stmt ...)
|#
