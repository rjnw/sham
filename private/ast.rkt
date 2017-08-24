#lang racket

(provide (all-defined-out))

(struct sham:module            (passes defs))


(struct sham:def ())

(struct sham:def:function  sham:def     (id passes attrs arg-ids arg-types ret-type body))
(struct sham:def:type      sham:def     (id type))


(struct sham:type ())

(struct sham:type:internal sham:type    ())
(struct sham:type:ref      sham:type    (to))
(struct sham:type:struct   sham:type    (fields types))
(struct sham:type:function sham:type    (args ret))
(struct sham:type:pointer  sham:type    (to))


(struct sham:stmt ())

(struct sham:stmt:let          (ids id-types id-vals stmt))
(struct sham:stmt:set!         (lhs val))
(struct sham:stmt:if           (tst thn els))
(struct sham:stmt:while        (tst body))
(struct sham:stmt:return       (val))
(struct sham:stmt:return-void  ())
(struct sham:stmt:block        (stmts))
(struct sham:stmt:exp          (e))


(struct sham:exp ())

(struct sham:exp:app           (rator rands))
(struct sham:exp:fl-value      (v t))
(struct sham:exp:si-value      (v t))
(struct sham:exp:ui-value      (v t))
(struct sham:exp:void-value    ())
(struct sham:exp:sizeof        (t))
(struct sham:exp:type          (t))
(struct sham:exp:gep           (ptr indxs))
(struct sham:exp:var           (v))


(struct sham:rator ())

(struct sham:rator:intrinsic   (str-id ret-type))
(struct sham:rator:symbol      (sym))

;;TODO add debug parameters to printer
(define (print-sham-type t)
  (match t
    [(sham:type:internal)
     '_]
    [(sham:type:ref to)
     to]
    [(sham:type:struct fields types)
     `(struct ,(for/list [(f fields) (t types)] `(,f ,(print-sham-type t))))]
    [(sham:type:function args ret)
     `(,@(map print-sham-type args) ,(print-sham-type ret))]
    [(sham:type:pointer to)
     `(* ,(print-sham-type to))]))
(define (print-sham-stmt stmt)
  (match stmt
    [(sham:stmt:let ids ts vs st)
     `(let ,(for/list [(i ids) (t ts) (v vs)] `(,i ,(print-sham-expr v)))
        ,(print-sham-stmt st))]
    [(sham:stmt:set! lhs val)
     `(set! ,(print-sham-expr lhs) ,(print-sham-expr val))]
    [(sham:stmt:if tst thn els)
     `(if ,(print-sham-expr tst) ,(print-sham-stmt thn) ,(print-sham-stmt els))]
    [(sham:stmt:while tst body)
     `(while ,(print-sham-expr tst) ,(print-sham-stmt body))]
    [(sham:stmt:return val)
     `(return ,(print-sham-expr val))]
    [(sham:stmt:return-void)
     `(return-void)]
    [(sham:stmt:block stmts)
     `(block ,@(map print-sham-stmt stmts))]
    [(sham:stmt:exp e)
     `(expr ,(print-sham-expr e))]))
(define (print-sham-expr e)
  (match e
    [(sham:exp:app rator rands)
     `(,(print-sham-rator rator) ,@(map print-sham-expr rands))]
    [(sham:exp:fl-value v t)
     `(float ,v ,(print-sham-type t))]
    [(sham:exp:si-value v t)
     `(sint ,v ,(print-sham-type t))]
    [(sham:exp:ui-value v t)
     `(uint ,v ,(print-sham-type t))]
    [(sham:exp:void-value)
     `void]
    [(sham:exp:sizeof t)
     `(sizeof ,(print-sham-type t))]
    [(sham:exp:type t)
     `(%type ,(print-sham-type t))]
    [(sham:exp:gep ptr indxs)
     `(gep ,(print-sham-expr ptr) ,@(map print-sham-expr indxs))]
    [(sham:exp:var v)
     v]))
(define (print-sham-rator r)
  (match r
    [(sham:rator:intrinsic str-id ret-type)
     `(intrinsice ,str-id)]
    [(sham:rator:symbol s)
     `(symbol ,s)]))
(define (print-sham-def def)
  (match def
    [(sham:def:function id passes attrs arg-ids arg-types ret-type body)
     `(define (,id ,@arg-ids)
        ,(print-sham-stmt body))]
    [(sham:def:type id t)
     `(define ,id ,(print-sham-type t))]))
(define (print-sham-ast ast)
  (match ast
    [(sham:module passes defs)
     `(module
       ,@(map print-sham-def defs))]))

(define (sham-type->sexp t)
  (match t
    [(sham:type:internal)
     '_]
    [(sham:type:ref to)
     to]
    [(sham:type:struct fields types)
     `(struct ,(for/list [(f fields) (t types)] `(,f : ,(sham-type->sexp t))))]
    [(sham:type:function args ret)
     `(,@(map sham-type->sexp args) -> ,(sham-type->sexp ret))]
    [(sham:type:pointer to)
     `(* ,(sham-type->sexp to))]))

(define (sham-stmt->sexp stmt)
  (match stmt
    [(sham:stmt:let ids ts vs st)
     `(let ,(for/list [(i ids) (t ts) (v vs)]
              `(,i : ,(sham-type->sexp t) ,(sham-expr->sexp v)))
        ,(sham-stmt->sexp st))]
    [(sham:stmt:set! lhs val)
     `(set! ,(sham-expr->sexp lhs) ,(sham-expr->sexp val))]
    [(sham:stmt:if tst thn els)
     `(if ,(sham-expr->sexp tst) ,(sham-stmt->sexp thn) ,(sham-stmt->sexp els))]
    [(sham:stmt:while tst body)
     `(while ,(sham-expr->sexp tst) ,(sham-stmt->sexp body))]
    [(sham:stmt:return val)
     `(return ,(sham-expr->sexp val))]
    [(sham:stmt:return-void)
     `(return-void)]
    [(sham:stmt:block stmts)
     `(block ,@(map sham-stmt->sexp stmts))]
    [(sham:stmt:exp e)
     `(expr ,(sham-expr->sexp e))]))

(define (sham-expr->sexp e)
  (match e
    [(sham:exp:app rator rands)
     `(,(sham-rator->sexp rator) ,@(map sham-expr->sexp rands))]
    [(sham:exp:fl-value v t)
     `(%float ,v ,(sham-type->sexp t))]
    [(sham:exp:si-value v t)
     `(%sint ,v ,(sham-type->sexp t))]
    [(sham:exp:ui-value v t)
     `(%uint ,v ,(sham-type->sexp t))]
    [(sham:exp:void-value)
     `void]
    [(sham:exp:sizeof t)
     `(%sizeof ,(sham-type->sexp t))]
    [(sham:exp:type t)
     `(%type ,(sham-type->sexp t))]
    [(sham:exp:gep ptr indxs)
     `(%gep ,(sham-expr->sexp ptr) ,@(map sham-expr->sexp indxs))]
    [(sham:exp:var v)
     v]))

(define (sham-rator->sexp r)
  (match r
    [(sham:rator:intrinsic str-id ret-type)
     `(,str-id :-> ,(sham-type->sexp ret-type))]
    [(sham:rator:symbol s)
     s]))

(define (sham-def->sexp def)
  (match def
    [(sham:def:function id passes attrs arg-ids arg-types ret-type body)
     `(define-function (passes ,@passes) (attrs ,@attrs)
        (,id ,@(for/list [(id arg-ids) (typ arg-types)]
                 `(,id : ,(sham-type->sexp typ)))
             : ,(sham-type->sexp ret-type))
        ,(sham-stmt->sexp body))]
    [(sham:def:type id t)
     `(define-type ,id ,(sham-type->sexp t))]))

(define (sham-ast->sexp ast)
  (match ast
    [(sham:module passes defs)
     `(module
          (passes ,@passes)
          ,@(map sham-def->sexp defs))]))

;;;---;;;
(define (sexp->sham-ast sexp)
  (match sexp
    [`(module
          (passes ,passes ...)
          ,defs ...)
     (sham:module passes (map sexp->sham-def defs))]))

(define (sexp->sham-type t)
  (match t
    ['_
     (sham:type:internal)]
    [`(* ,to)
     (sham:type:pointer (sexp->sham-type to))]
    [`(,args ... -> ,ret)
     (sham:type:function (map sexp->sham-type args) (sexp->sham-type ret))]
    [`(struct ((,fields : ,types) ...))
     (sham:type:struct fields (map sexp->sham-type types))]
    [to #:when (symbol? to)
     (sham:type:ref to)]))

(define (sexp->sham-stmt sexp)
  (match sexp
    [`(let ((,ids : ,types ,vals) ...) ,stmt)
     (sham:stmt:let ids (map sexp->sham-type types) (map sexp->sham-expr vals)
                    (sexp->sham-stmt stmt))]
    [`(set! ,lhs ,val)
     (sham:stmt:set! (sexp->sham-expr lhs) (sexp->sham-expr val))]
    [`(if ,tst ,thn ,els)
     (sham:stmt:if (sexp->sham-expr tst) (sexp->sham-stmt thn) (sexp->sham-stmt els))]
    [`(while ,tst ,body)
     (sham:stmt:while (sexp->sham-expr tst) (sexp->sham-stmt body))]
    [`(return ,val)
     (sham:stmt:return (sexp->sham-expr val))]
    [`(return-void)
     (sham:stmt:return-void)]
    [`(block ,stmts ...)
     (sham:stmt:block (map sexp->sham-stmt stmts))]
    [`(expr ,e)
     (sham:stmt:exp (sexp->sham-expr e))]))

(define (sexp->sham-expr e)
  (match e
    [`(%float ,v ,t)
     (sham:exp:fl-value v (sexp->sham-type t))]
    [`(%sint ,v ,t)
     (sham:exp:si-value v (sexp->sham-type t))]
    [`(%uint ,v ,t)
     (sham:exp:ui-value v (sexp->sham-type t))]
    ['void
     (sham:exp:void-value)]
    [`(%sizeof ,t)
     (sham:exp:sizeof (sexp->sham-type t))]
    [`(%type ,t)
     (sham:exp:type (sexp->sham-type t))]
    [`(%gep ,ptr ,indxs ...)
     (sham:exp:gep (sexp->sham-expr ptr) (map sexp->sham-expr indxs))]
    [`(,rator ,rands ...)
     (sham:exp:app (sexp->sham-rator rator) (map sexp->sham-expr rands))]
    [v #:when (symbol? v)
       (sham:exp:var v)]))

(define (sexp->sham-rator r)
  (match r
    [`(,str-id :-> ,ret-type)
     (sham:rator:intrinsic str-id (sexp->sham-type ret-type))]
    [s #:when (symbol? s)
       (sham:rator:symbol s)]))

(define (sexp->sham-def def)
  (match def
    [`(define-function (passes ,passes ...)
        (attrs ,attrs ...)
        (,id (,arg-ids : ,arg-types) ... : ,ret-type)
        ,body)
     (sham:def:function id passes attrs arg-ids (map sexp->sham-type arg-types)
                        (sexp->sham-type  ret-type)
                        (sexp->sham-stmt body))]
    [`(define-type ,id ,t)
     (sham:def:type id (sexp->sham-type t))]))


;;utils
(define-syntax (sham$block stx)
  (syntax-case stx ()
    [(_ stmts ...)
     #'(sham:stmt:block (list stmts ...))]))
(define sham$var sham:exp:var)
(define-syntax (sham$app stx)
  (syntax-case stx ()
    [(_ (str t) rands ...)
     #'(sham:exp:app (sham:rator:intrinsic 'str
                                           (sham:type:ref 't))
                     (list rands ...))]
    [(_ sym rands ...)
     #'(sham:exp:app (sham:rator:symbol 'sym) (list rands ...))]))
(define-syntax (sham$app-var stx)
  (syntax-case stx ()
    [(_ app rands ...)
     #'(sham$app app (sham:exp:var 'rands) ...)]))
(define-syntax (sham$define stx)
  (syntax-case stx (return :)
    [(_ (id (args : t) ... : rett) (return stmt))
     #'(sham:def:function
        'id '() '(AlwaysInline)
        '(args ...) (list (sham:type:ref 't) ...) (sham:type:ref 'rett)
        (sham:stmt:return stmt))]))
