#lang racket
(require "../ast.rkt"
         sham/sam/recursion)
(require syntax/parse)
(provide (all-defined-out))

(build-recursion (stx-to-cry-stx)
                 (rkt-syntax -> (rkt-syntax cry-ast))
                 (top (stx -> stx)
                      [('def t:def ds:expr ...) (def:val t ds ...)]
                      [('test name:id ('== a:expr b:cexpr)) (def:test name a b)])
                 (def (stx -> stx)
                   [['type name:id '= ts:type] (def:ttype name ts)]
                   [[name:id ': ts:type] (def:vtype name ts)]
                   [[name:id ps:pat ... '= b:cexpr ws:def ...] (def:bind name (ps ...) (expr:where b ws ...))])
                 (pat (stx -> stx)
                      [name:id (pat:var name)]
                      [#(ps:pat ...) (pat:typ ps ...)]
                      [[ps:pat ...] (pat:seq ps ...)])

                 (cexpr (stx -> stx)
                        [[es:cexpr ...] (sequence:basic es ...)]
                        [#(es:cexpr ...) (expr:tuple es ...)]
                        [(': e:cexpr t:type) (expr:annot e t)]
                        [('cond [chk:cexpr thn:cexpr] ... ['else els:cexpr]) (expr:ifcond (chk thn) ... els)]
                        [(rator:cexpr rands:cexpr ...) (expr:app rator rands ...)])

                 (stype (stx -> stx)
                        [(~seq ffrom:type (~seq '-> tto:type) ...)
                         (type:func ffrom (list tto ...))]
                        [t:type t]
                        [(t:type) t]
                        #:splicing)

                 (type (stx -> stx)
                       [({pvars:id ...} cs:constraint ... '=> ts:stype) (type:poly (pvars ...) (cs ...) ts)]
                       [(ffrom:type '-> tto:type) (type:func ffrom tto)]
                       [#(t:type ...) (type:tuple t ...)]
                       [[dm:dim t:type] (type:sequence dm t)]
                       [[dm:dim] (type:sequence dm type:bit)]
                       ;; #:splicing
                       )
                 (dim (stx -> stx)
                      [i:integer i]
                      [n:id n])
                 (constraint (stx -> stx)
                             [(op:id vals:expr ...) #`(cls op vals ...)])
                 #:strict-parens)
