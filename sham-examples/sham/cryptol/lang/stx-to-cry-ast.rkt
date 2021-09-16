#lang racket
(require "../ast.rkt"
         sham/sam/transform)
(require syntax/parse)
(provide (all-defined-out))

(define-transform
 (stx-to-cry-ast)
 (rkt-syntax -> cry-ast)

 (top (stx -> def)
      [('def t:def ds:def ...) (val t ds ...)]
      [('test name:id ('== a:cexpr b:cexpr)) (test name a b)]
      [('type [name:id '= t:stype]) (type name t)])

 (def (stx -> def)
   [['type name:id '= ts:stype] (type name ts)]
   [[name:id ': ~! ts:stype] (typeof name ts)]
   [[name:id ps:pat ... '= ~! b:cexpr ws:def ...] (bind name (ps ...) (expr:where b ws ...))])

 (pat (stx -> pat)
      [(~and (~not '=) name:id) (var name)]
      [#(ps:pat ...) (tuple ps ...)]
      [[ps:pat ...] (sequence ps ...)])

 (cexpr (stx -> expr)
        [[v:cexpr (~seq '\| p:pat '<- pv:cexpr) ...] (sequence:comp v ((p pv)) ...)]
        [[v1:cexpr '.. v2:cexpr] (sequence:enum v1 v2)]
        [[es:cexpr ...] (sequence:basic es ...)]
        [s:string (sequence:str s)]

        [#(es:cexpr ...) (tuple es ...)]
        [(': e:cexpr t:type) (annot e t)]
        [('cond [chk:cexpr thn:cexpr] ... ['else els:cexpr]) (cond (chk thn) ... els)]
        [('quote n:id) (tvar n)]
        [('error msg:string) (error msg)]
        [(rator:cexpr {iargs:cexpr ...} rands:cexpr ...)
         (app rator (iargs ...) rands ...)]
        [(rator:cexpr rands:cexpr ...) (app rator () rands ...)]
        [n:id (var n)]
        [i:integer (lit i)]
        [c:char (char c)])

 (type (stx -> type)
       [#(t:type ...) (tuple t ...)]
       [[dm:dim t:type] (sequence dm t)]
       [[dm:dim] (sequence dm type:bit)]
       [n:id (var n)]
       ['integer integer]
       ['bit bit]
       [(st:stype) st])

 (stype (stx -> type)
        [(~seq {pvars:id ...} ~! ts:stype) (poly (pvars ...) ts)]
        [(~seq (~and (~not '=>) cs:expr) ... '=> ~! ts:stype) (constraint (cs ...) ts)]
        [(~seq ffrom:type '-> ~! tto:stype) (func ffrom tto)]
        [(t:stype) t]
        [t:type t]
        #:splicing)

 (dim (stx -> dim)
      [i:integer i]
      [(rator:id rands:dim ...) (app rator (rands ...))]
      [n:id (var n)])
 #:strict-parens)
