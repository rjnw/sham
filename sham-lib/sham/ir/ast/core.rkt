#lang racket

(require sham/astg/ast)

(provide (all-defined-out))

(define-ast sham
  #:custom-write #t
  (def
    [module (defs:def ...)]
    [function (type body:stmt)]
    [struct ((field-name field-type:type) ...)]
    [racket (value type)]
    #:common-mutable info
    #:common id)
  (ast #:common-auto-mutable metadata)
  (rator ast
         [reference id]
         [llvm      id]
         [intrinsic (id type)]
         [external  (lib-id id type var-arg?)])
  (stmt ast
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
  (expr ast
        [ref      sym]
        [op       (rator flags args ...)]
        [access   (struct-field value)]
        [void     ()]
        [etype    (t:type)]
        [let      (((ids:terminal.sym vals:expr types:type)
                    ...)
                   stmt:stmt
                   expr:expr)]))
