#lang racket

(require "ir/ast.rkt"
         sham/sam/id)

(struct context [current bindings])

(define ((register-in-context ctxt decl) val)
  (define id (post:decl-id decl))
  ;; check val and add to appropriate map
  (match-define (context crnt bnds) ctxt)
  (context crnt (id-binding-map-add bnds id val)))
(define (to-module-context . args) 'TODO)
(define (to-instance-context . args) 'TODO)
(define (do-register-in-context ctxt id val)
  (values val (register-in-context ctxt id val)))
(define (lookup-identifier id ctxt) 'TODO)

(struct ivalue [])
(struct idecl ivalue [id])
(struct ideclmod idecl [isig idecls])
(struct ideclclass idecl [iargs ifields])
(struct ideclval idecl [itype interpretf])

(struct iexpr ivalue [])
(struct idata iexpr [subtype ifields])
(struct ilit iexpr [itype value])

(struct itype [type ctxt])

(define-compiler (interpret-post #:state (ctxt (initial-context))
                                 #:read (type #f))
  (post -> rkt)
  (cdecl (decl -> def)
         #:post (register-in-context ctxt decl)
         [(post:decl:mod id sig ds ...)
          #:compile [sig^ sig]
          #:state [ctxt (to-module-context ctxt id sig^)]
          #:compile [(ds^ ...) (ds ...)]
          (ideclmod id sig^ (build-decl-map ds^))]
         [(post:decl:typ id type)
          #:comiple [type^ type]
          (idecltype id type^)]
         [(post:decl:val id type body)
          #:compile [type^ type]
          #:compile [body^ body]
          (ideclvalue id type^ body^)])
  (cexpr (expr -> any)
         [(post:expr:var id)
          (lookup-identifier id ctxt)]
         [(post:expr:app op args ...)
          TODO]
         [(post:expr:data type fields ...)
          #:compile [type^ type]
          #:compile [fields^ (map cexpr fields (type-fields type^))]
          (idata (lookup-subtype t itype) (ie fields))]
         [(post:expr:anno val typ)
          #:compile [typ^ typ]
          #:compile [val^ val #:reader [type typ^]]
          val^]
         [(post:expr:case (args ...) (pats ... body) ... dflt)
          TODO]
         [(post:expr:lit typ val)
          #:compile [typ^ typ]
          (ilit typ^ val)] )
  (ctype (type -> any)
         [(post:type:sig (fld typ) ...)]
         [(post:type:adt (subi subt ...) ...)]
         [(post:type:fun args ... ret)]
         [(post:type:lit sham check coerce)]))
