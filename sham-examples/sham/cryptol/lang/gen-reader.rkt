#lang racket
(require "../ast.rkt"
         sham/sam/tranformation)

(build-transformer
 (cry-read-one-syntax)
 (rkt-syntax -> (rkt-syntax cry-ast))
 (top
  [('def t:def ds:def ...) (def:val t ds)]
  [('test name:id ('== a:expr b:expr)) (def:test name a b)])
 (def
   [[name:id ps:pat ... '= b:expr ws:def ...] (def:bind name ps (expr:where b ws))]
   [[name:id ': ts:type] (def:vtype name ts)]
   [['type name:id '= ts:type] (def:ttype name ts)])
 (pat
  [name:id (pat:var name)]
  [#(ps:pat ...) (pat:typ ps)]
  [[ps:pat ...] (pat:seq ps)])

 (expr
  [[es:expr ...] (sequence:basic es)]
  [#(es:expr ...) (expr:tuple es)]
  [(': e:expr t:type) (expr:annot e t)]
  [('cond [chk:expr thn:expr] ... ['else els:expr])
   (expr:ifcond (chk thn) ... els)]
  [(rator:expr rands:expr ...)
   (expr:app rator rands ...)])

 (type
  [(~seq (~optional {pvars:id ...})
         (~optional cs:constraint ... '=>)
         ts:type)
   (type:poly (pvars ...) (cs ...) ts)]
  [(~seq ffrom:type (~seq '-> tto:type) ...) (type:func ffrom tto)]
  [#(t:type ...) (type:tuple t ...)]
  [[ts:type ...] (type:sequence ts ..)]))
