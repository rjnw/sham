#lang racket
(require "../../rcf/private/compiler.rkt")
(provide (all-defined-out))

(define-ast sham
   (def
     [module        (defs:def ...)]
     [function      ((arg-ids:terminal.sym arg-types:type) ... ret-type:type body:stmt)]
     [type          (type:type)]
     [global        (type:type)]
     [global-string (str:terminal.string)]
     #:common-mutable info
     #:common id:terminal.sym)
   (ast #:common-auto-mutable metadata)
   (type ast
         [internal ()]
         [ref      (to:terminal.sym)]
         [struct   ((fields:terminal.sym types:type) ...)]
         [function (args:type ... '-> ret:type)]
         [pointer  (to:type)]
         [array    (of:type size:terminal.unsigned-int)]
         [vector   (of:type size:terminal.unsigned-nt)])
   (rator ast
          ;; ;; Symbol for definitions
          [symbol    id:terminal.sym]
          ;; ;; LLVM implementation hooks
          [intrinsic (id:terminal.sym return-type:type)]
          ;; ;; Shared object names
          [external  (lib-id:terminal.sym id:terminal.sym ret-type:type)]
          ;; ;; calls back into racket from generated code
          [racket    (id:terminal.sym racket-value:terminal.rkt full-type:type)])
   (stmt ast
         [set!     (lhs:expr.var val:expr)]
         [if       (test:expr then:stmt else:stmt)]
         [switch   (test:expr (check:expr body:stmt) ... default:expr)]
         [break    ()]
         [while    (test:expr body:stmt)]
         [return   (value:expr)]
         [void     ()]
         [expr     (e:expr)]
         [block    (stmts:stmt ...)])
   (expr ast
         [app      (rator:expr rands:expr ...)]
         [void     ()]
         [sizeof   (t:type)]
         [etype    (t:type)]
         [gep      (pointer:expr indexes:expr ...)]
         [var      id:terminal.sym]
         [global   (id:terminal.sym)]
         [external (lib-id:terminal.sym id:terminal.sym t:type)]
         [let      (((ids:terminal.sym vals:expr types:type)
                     ...)
                    stmt:stmt
                    expr:expr)])
   (const expr
          [fl     (value:terminal.float        type:type)]
          [si     (value:terminal.signed-int   type:type)]
          [ui     (value:terminal.unsigned-int type:type)]
          [string (value:terminal.string)]
          [llvm   (value:terminal.llvm         type:type)]
          [struct (value:terminal.struct       type:type)]
          [array  (value:terminal.array        type:type)]
          [vector (value:terminal)])
   (terminal #:terminals
             [sym symbol?]
             [float fixnum?]
             [signed-int exact-integer?]
             [unsigned-int exact-nonnegative-integer?]
             [string sham-string?]
             [llvm llvm?]
             [struct sham-struct?]
             [array sham-array?]
             [vector sham-vector?])
   ;; (terminal
   ;;  [float v #:native real?]
   ;;  [signed-int v #:native exact-positive-integer?]
   ;;  [unsigned-int v #:native exact-integer?]
   ;;  [string v #:native string?]
   ;;  [llvm   v]
   ;;  [struct v]
   ;;  [array  v]
   ;;  [vector v])
   )


;; generated structs
#;((struct sham:def:module sham:def (defs:def))
   (struct sham:def:function sham:def (arg-ids:terminal.sym arg-types:type ret-type:type body:stmt))
   (struct sham:def:type sham:def (type:type))
   (struct sham:def:global sham:def (type:type))
   (struct sham:def:global-string sham:def (str:terminal.string))
   (struct sham:ast ((metadata #:auto)))
   (struct sham:ast:type sham:ast ())
   (struct sham:ast:type:internal sham:ast:type ())
   (struct sham:ast:type:ref sham:ast:type (to:terminal.sym))
   (struct sham:ast:type:struct sham:ast:type (fields:terminal.sym types:type))
   (struct sham:ast:type:function sham:ast:type (args:type ret:type))
   (struct sham:ast:type:pointer sham:ast:type (to:type))
   (struct sham:ast:type:array sham:ast:type (of:type size:terminal.unsigned-int))
   (struct sham:ast:type:vector sham:ast:type (of:type size:terminal.unsigned-nt))
   (struct sham:ast:rator sham:ast ())
   (struct sham:ast:rator:symbol sham:ast:rator (id:terminal.sym))
   (struct sham:ast:rator:intrinsic sham:ast:rator (id:terminal.sym return-type:type))
   (struct sham:ast:rator:external sham:ast:rator (lib-id:terminal.sym id:terminal.sym ret-type:type))
   (struct sham:ast:rator:racket sham:ast:rator (id:terminal.sym racket-value:terminal.rkt full-type:type))
   (struct sham:ast:stmt sham:ast ())
   (struct sham:ast:stmt:set! sham:ast:stmt (lhs:expr.var val:expr))
   (struct sham:ast:stmt:if sham:ast:stmt (test:expr then:stmt else:stmt))
   (struct sham:ast:stmt:switch sham:ast:stmt (test:expr check:expr body:stmt default))
   (struct sham:ast:stmt:break sham:ast:stmt ())
   (struct sham:ast:stmt:while sham:ast:stmt (test:expr body:stmt))
   (struct sham:ast:stmt:return sham:ast:stmt (value:expr))
   (struct sham:ast:stmt:void sham:ast:stmt ())
   (struct sham:ast:stmt:expr sham:ast:stmt (e:expr))
   (struct sham:ast:stmt:block sham:ast:stmt (stmts:stmt))
   (struct sham:ast:expr sham:ast ())
   (struct sham:ast:expr:app sham:ast:expr (rator:expr rands:expr))
   (struct sham:ast:expr:void sham:ast:expr ())
   (struct sham:ast:expr:sizeof sham:ast:expr (t:type))
   (struct sham:ast:expr:etype sham:ast:expr (t:type))
   (struct sham:ast:expr:gep sham:ast:expr (pointer:expr indexes:expr))
   (struct sham:ast:expr:var sham:ast:expr (id:terminal.sym))
   (struct sham:ast:expr:global sham:ast:expr (id:terminal.sym))
   (struct sham:ast:expr:external sham:ast:expr (lib-id:terminal.sym id:terminal.sym t:type))
   (struct sham:ast:expr:let sham:ast:expr (ids:terminal.sym vals:expr types:type stmt:stmt expr:expr))
   (struct sham:ast:expr:const sham:ast:expr ())
   (struct sham:ast:expr:const:fl sham:ast:expr:const (value:terminal.float type:type))
   (struct sham:ast:expr:const:si sham:ast:expr:const (value:terminal.signed-int type:type))
   (struct sham:ast:expr:const:ui sham:ast:expr:const (value:terminal.unsigned-int type:type))
   (struct sham:ast:expr:const:string sham:ast:expr:const (value:terminal.string type:type))
   (struct sham:ast:expr:const:llvm sham:ast:expr:const (value:terminal.llvm type:type))
   (struct sham:ast:expr:const:struct sham:ast:expr:const (value:terminal.struct type:type))
   (struct sham:ast:expr:const:array sham:ast:expr:const (value:terminal.array type:type))
   (struct sham:ast:expr:const:vector sham:ast:expr:const (value:terminal.vector type:type)))
