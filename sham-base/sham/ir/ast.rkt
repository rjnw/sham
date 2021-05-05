#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/md
         sham/llvm/ir/ast)

(provide (all-defined-out))

(define-ast sham
  (def
    [module (defs:def ...)]
    [function (type body:stmt)]
    [struct ((field-names:id field-types:type) ...)]
    [racket (value type rkt-type)]
    #:common id:id)
  (rator
   [reference (id)]
   [llvm      (id)]
   [intrinsic (id type)]
   [external  (lib-id id type var-arg?)])
  (stmt
   [set!     (lhs val:expr)]
   [if       (test:expr then:stmt else:stmt)]
   [switch   (test:expr (check:expr body:stmt) ... default:expr)]
   [continue ()]
   [break    ()]
   [while    (test:expr body:stmt)]
   [void     ()]
   [expr     (e:expr)]
   [block    (stmts:stmt ...)]
   [label    (name body:stmt)]
   [return   (value)]
   [return-void ()])
  (expr
   [ref      (sym)]
   [op       (rator flags args ...)]
   [access   (struct-field value)]
   [void     ()]
   [etype    (t:type)]
   [let      (((ids:id vals:expr types:type) ...) stmt:stmt expr:expr)])
  #:with struct-helpers
  #:default-metadata (empty-md))

(define sham-metadata llvm-metadata)
(define sham-md sham-metadata)
