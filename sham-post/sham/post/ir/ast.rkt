#lang racket

(require sham/sam/ast)

(provide (all-defined-out))

(define-ast post
  (decl
   [mod (sig:type.sig ds:decl ...)]
   [typ (t:type)]
   [val (t:type body:expr)]
   #:common id:id)
  (expr
   [var id]
   [app (op:expr args:expr ...)]
   [data (t:type fields ...)]
   [anno (v:expr t:type)]
   [case ((args:expr ...) (pats:pattern ... body:expr) ... dflt:expr)]
   [lam ((args:id ...) body:expr)]
   [lit (val t:type)])
  (pattern
   [sub (which ps:pat ...)]
   [val (v:expr)]
   [all ()]
   [chk (c?)]
   #:common id:id)
  (type
   [sig ((field:id t:type) ...)]        ;; signature of a module
   [adt ((subi:id subt:type ...) ...)]
   [fun (args:type ... ret:type)]
   [lit (sham check coerce)]))
