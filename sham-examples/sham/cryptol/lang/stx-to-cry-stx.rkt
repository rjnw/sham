#lang racket
(require "../ast.rkt"
         sham/sam/recursion)
(require syntax/parse)
(provide (all-defined-out))

(build-recursion (stx-to-cry-stx)
                 (rkt-syntax -> (rkt-syntax cry-ast))
                 (top (stx -> stx)
                      [('def t:def ds:def ...) (def:val t ds ...)]
                      [('test name:id ('== a:cexpr b:cexpr)) (def:test name a b)]
                      [('type [name:id '= t:stype])
                       (def:type name t)])
                 (def (stx -> stx)
                   [['type name:id '= ts:stype] (def:ttype name ts)]
                   [[name:id ': ~! ts:stype] (def:vtype name ts)]
                   [[name:id ps:pat ... '= ~! b:cexpr ws:def ...] (def:bind name (ps ...) (expr:where b ws ...))])
                 (pat (stx -> stx)
                      [(~and (~not '=) name:id) (pat:var name)]
                      [#(ps:pat ...) (pat:tup ps ...)]
                      [[ps:pat ...] (pat:seq ps ...)])

                 (cexpr (stx -> stx)
                        [[v:cexpr (~seq '\| p:pat '<- pv:cexpr) ...] (sequence:comp v (p pv) ...)]
                        [[v1:cexpr '.. v2:cexpr] (sequence:enum v1 v2)]
                        [[es:cexpr ...] (sequence:basic es ...)]
                        [#(es:cexpr ...) (expr:tuple es ...)]
                        [(': e:cexpr t:type) (expr:annot e t)]
                        [('cond [chk:cexpr thn:cexpr] ... ['else els:cexpr]) (expr:ifcond (chk thn) ... els)]
                        [('quote n:id) (expr:tvar n)]
                        [('error msg:string) (expr:error msg)]
                        [(rator:cexpr {iargs:cexpr ...} rands:cexpr ...)
                         (expr:app rator (iargs ...) rands ...)]
                        [(rator:cexpr rands:cexpr ...) (expr:app rator () rands ...)]
                        [n:id (expr:var n)]
                        [i:integer (expr:lit i)]
                        [s:string (sequence:str s)]
                        [c:char (expr:char c)])

                 (type (stx -> stx)
                       [#(t:type ...) (type:tuple t ...)]
                       [[dm:dim t:type] (type:sequence dm t)]
                       [[dm:dim] (type:sequence dm type:bit)]
                       [n:id (type:var n)]
                       ['integer (type:integer)]
                       [(st:stype) st])

                 (stype (stx -> stx)
                        [(~seq {pvars:id ...} ~! ts:stype) (type:poly (pvars ...) ts)]
                        [(~seq (~and (~not '=>) cs:expr) ... '=> ~! ts:stype) (type:constraint (cs ...) ts)]
                        [(~seq ffrom:type '-> ~! tto:stype) (type:func ffrom tto)]
                        [(t:stype) t]
                        [t:type t]
                        #:splicing)

                 (dim (stx -> stx)
                      [i:integer i]
                      [(rator:id rands:dim ...) (dim:app rator (rands ...))]
                      [n:id n])
                 #:strict-parens)
