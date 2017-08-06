#lang racket

(provide (all-defined-out))

(define-struct sham:module            (passes defs))


(define-struct sham:def ())

(define-struct (sham:def:function sham:def)      (id passes attrs arg-ids arg-types ret-type body))
(define-struct (sham:def:type sham:def)          (id type))


(define-struct sham:type ())

(define-struct (sham:type:internal sham:type)     ())
(define-struct (sham:type:ref sham:type)          (to))
(define-struct (sham:type:struct sham:type)       (fields types))
(define-struct (sham:type:function sham:type)     (args ret))
(define-struct (sham:type:pointer sham:type)      (to))


(define-struct sham:stmt ())

(define-struct sham:stmt:let          (ids id-types id-vals stmt))
(define-struct sham:stmt:set!         (lhs val))
(define-struct sham:stmt:if           (tst thn els))
(define-struct sham:stmt:while        (tst body))
(define-struct sham:stmt:return       (val))
(define-struct sham:stmt:return-void  ())
(define-struct sham:stmt:block        (stmts))
(define-struct sham:stmt:exp          (e))


(define-struct sham:exp ())

(define-struct sham:exp:app           (rator rands))
(define-struct sham:exp:fl-value      (v t))
(define-struct sham:exp:si-value      (v t))
(define-struct sham:exp:ui-value      (v t))
(define-struct sham:exp:void-value    ())
(define-struct sham:exp:sizeof        (t))
(define-struct sham:exp:type          (t))
(define-struct sham:exp:gep           (ptr indxs))
(define-struct sham:exp:var           (v))


(define-struct sham:rator ())

(define-struct sham:rator:intrinsic   (str-id ret-type))
(define-struct sham:rator:symbol      (sym))

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
     str-id]
    [(sham:rator:symbol s)
     s]))
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

;; (define (ast->sexp ast)
;;   (define as ast->sexp)
;;   (match ast
;;     [(ast-module defs)
;;      `(#%module
;;        ,@(map as defs))]
;;     [(ast-function-def id passes attrs arg-ids arg-types ret-type body)
;;      `(define-function (#:pass ,@passes) (#:attr ,@attrs)
;;         (,id ,@(for/list ([i arg-ids]
;;                           [t arg-types])
;;                  `(,(as i) : ,t)) : ,ret-type)
;;         ,(as body))]
;;     [(ast-type-def id type)
;;      `(define-type ,id ,(as type))]

;;     [(ast-type-ref to)
;;      to]
;;     [(ast-type-struct fields types)
;;      `(struct ,@(for/list ([i fields] [t types]) `(,i : ,t)))]
;;     [(ast-type-function args ret)
;;      `(,@args -> ,ret)]
;;     [(ast-type-pointer to)
;;      `(pointer ,to)]

;;     [(ast-stmt-let id id-type id-val body)
;;      `(let ((,(as id) : ,id-type ,(as id-val)))
;;         ,(as body))]
;;     [(ast-stmt-set! lhs val)
;;      `(set! ,(as lhs) ,(as val))]
;;     [(ast-stmt-store! lhs val)
;;      `(store! ,(as lhs) ,(as val))]
;;     [(ast-stmt-if tst thn els)
;;       `(if ,(as tst) ,(as thn) ,(as els))]
;;     [(ast-stmt-while phi-vars phi-types test body)
;;      `(while ,(for/list ([p phi-vars]
;;                          [t phi-types])
;;                 `(,(as p) : ,t))
;;         ,(as test) ,(as body))]
;;     [(ast-stmt-ret val)
;;      `(return ,(as val))]
;;     [(ast-stmt-ret-void)
;;      `(return-void)]
;;     [(ast-stmt-block bodys)
;;      `(block ,@(map as bodys))]
;;     [(ast-stmt-exp val)
;;      `(#%exp ,(as val))]

;;     [(ast-exp-app rator rands)
;;      `(#%app ,(as rator) ,@(map as rands))]
;;     [(ast-exp-fl-value v t)
;;      `(#%fl-value ,(as v) ,(as t))]
;;     [(ast-exp-si-value v t)
;;      `(#%si-value ,(as v) ,(as t))]
;;     [(ast-exp-ui-value v t)
;;      `(#%ui-value ,(as v) ,(as t))]
;;     [(ast-exp-void-value)
;;      '#%void]
;;     [(ast-exp-sizeof t)
;;      `(#%sizeof ,(as t))]
;;     [(ast-exp-type t)
;;      `(#%type ,t)]
;;     [(ast-exp-gep ptr indxs)
;;      `(#%gep ,(as ptr) ,(map as indxs))]
;;     [(ast-exp-var v)
;;      v]
;;     [else ast]))

;; (define (sexp->ast s)
;;   (define sa sexp->ast)
;;   (match s
;;     [`(define-function (#:pass ,passes ...) (#:attr ,attrs ...)
;;         (,id (,arg-ids : ,arg-types) ... : ,ret-type)
;;         ,body)
;;      (ast-function-def id passes attrs arg-ids arg-types ret-type body)]
;;     [`(define-type ,id (struct (,fields : ,types)))
;;      (ast-type-def id (ast-type-struct fields types))]
;;     [`(define-type ,id (,args ... -> ,ret))
;;      (ast-type-def id (ast-type-function args ret))]
;;     [`(define-type ,id (pointer ,to))
;;      (ast-type-def id (ast-type-pointer to))]
;;     [`(define-type ,id ,to) #:when (symbol? to)
;;      (ast-type-def (ast-type-ref to))]
;;     [`(let ((,ids : ,types ,vals) ...) ,st)
;;      (ast-stmt-let ids types (map sa vals) (sa st))]
;;     [`(set! ,lhs ,v)
;;      (ast-stmt-set! lhs (sa v))]
;;     [`(store! ,lhs ,v)
;;      (ast-stmt-store! lhs (sa v))]
;;     [`(if ,tst ,thn ,els)
;;      (ast-stmt-if (sa tst) (sa thn) (sa els))]
;;     [`(while ((,vars : ,var-types) ...) ,tst ,body)
;;      (ast-stmt-while vars var-types (sa tst) (sa body))]
;;     [`(return ,v)
;;      (ast-stmt-ret (sa v))]
;;     [`(return-void)
;;      (ast-stmt-ret-void)]
;;     [`(block ,stmts ...)
;;      (ast-stmt-block (map sa stmts))]
;;     [`(#%exp ,e)
;;      (ast-stmt-exp (sa e))]
;;     [`(#%app ,rator ,rands ...)
;;      (ast-exp-app (sa rator) (map sa rands))]
;;     [`(#%fl-value ,value ,type)
;;      (ast-exp-fl-value value (sa type))]
;;     [`(#%si-value ,value ,type)
;;      (ast-exp-si-value value (sa type))]
;;     [`(#%ui-value ,value ,type)
;;      (ast-exp-ui-value value (sa type))]
;;     [`(#%sizeof ,type)
;;      (ast-exp-sizeof (sa type))]
;;     [`(#%type ,t)
;;      (ast-exp-type t)]
;;     [`(#%gep ,ptr ,indxs)
;;      (ast-exp-gep (sa ptr) (map sa indxs))]
;;     [(? symbol?)
;;      (ast-exp-var s)]))



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
