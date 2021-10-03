#lang racket
(require "../ast.rkt"
         sham/sam/transform
         sham/sam/runtime)

(require syntax/parse)
(provide (all-defined-out))

(define-transform
  (stx-to-cry-ast)
  (rkt-syntax -> cry-ast)

  (top (stx -> def)
       [('def ds:def ...)
        ;; #:bind-ids [ds -> *]
        (make gen ds)]
       [('test (id-def name 'test) ('== a:cexpr b:cexpr)) (make test name a b)]
       [('type [(id-def name 'type) '= t:stype]) (make type (id-create-ref name) t)])

  (def (stx -> def)
    [['type (id-def name 'type) '= ts:stype] (make type name ts)]
    [[(id-def name 'value) ': ~! ts:stype] (make typeof (id-create-ref name) ts)]
    [[(id-def name 'value) ps:pat ... '= ~! b:cexpr ws:def ...]
     ;; #:bind-ids [ps -> ws b] [ws -> * b]
     (make val name (make expr:bind ps (make expr:where b ws)))])

  (pat (stx -> pat)
       [(~and (~not '=) (id-def name 'value)) (make var name)]
       [#(ps:pat ...) (make tuple ps)]
       [[ps:pat ...] (make sequence ps)])

  (cexpr (stx -> expr)
         [[v:cexpr (~seq '\| p:pat '<- pv:cexpr) ...] (make sequence:comp v p pv)]
         [[v1:cexpr '.. v2:cexpr] (make sequence:enum v1 (make lit 1) v2)]
         [[es:cexpr ...] (make sequence:basic es)]
         [s:string (make sequence:str s)]

         [#(es:cexpr ...) (make tuple es)]
         [(': e:cexpr t:type) (make annot e t)]
         [('cond [chk:cexpr thn:cexpr] ... ['else els:cexpr]) (make cond chk thn els)]
         [('quote (id-ref name 'type)) (make tvar name)]
         [('error msg:string) (make error msg)]
         [(rator:cexpr {iargs:cexpr ...} rands:cexpr ...)
          (make app rator iargs rands)]
         [(rator:cexpr rands:cexpr ...) (make app rator '() rands)]
         [(id-ref name 'value) (make var name)]
         [i:integer (make lit i)]
         [c:char (make char c)])

  (type (stx -> type)
        [#(t:type ...) (make tuple t)]
        [[dm:dim t:type] (make sequence dm t)]
        [[dm:dim] (make sequence dm (make bit))]
        [(id-ref name 'type) (make var name)]
        ['integer (make integer)]
        ['bit (make bit)]
        [(st:stype) st])

  (stype (stx -> type)
         [(~seq {(id-def pvars 'type) ...} ~! ts:stype)
          ;; #:bind-ids [pvars -> ts]
          (make poly pvars ts)]
         [(~seq (~and (~not '=>) cs:expr) ... '=> ~! ts:stype) (make constraint cs ts)]
         [(~seq ffrom:type '-> ~! tto:stype) (make func ffrom tto)]
         [(t:stype) t]
         [t:type t]
         #:splicing)

  (dim (stx -> dim)
       [i:integer (make int i)]
       [(rator:id rands:dim ...) (make app rator rands)]
       [(id-ref name 'type) (make var name)])
  #:strict-parens)
