#lang racket

(provide (all-defined-out))

(define-struct sham:module            (passes defs))

(define-struct sham:def:function      (id passes attrs arg-ids arg-types ret-type body))
(define-struct sham:def:type          (id type))

(define-struct sham:type:ref          (to))
(define-struct sham:type:struct       (fields types))
(define-struct sham:type:function     (args ret))
(define-struct sham:type:pointer      (to))

(define-struct sham:stmt:let          (ids id-types id-vals stmt))
(define-struct sham:stmt:set!         (lhs val))
(define-struct sham:stmt:store!       (lhs val))
(define-struct sham:stmt:if           (tst thn els))
(define-struct sham:stmt:while        (tst body))
(define-struct sham:stmt:return       (val))
(define-struct sham:stmt:return-void  ())
(define-struct sham:stmt:block        (stmts))
(define-struct sham:stmt:exp          (e))

(define-struct sham:exp:app           (rator rands))
(define-struct sham:exp:fl-value      (v t))
(define-struct sham:exp:si-value      (v t))
(define-struct sham:exp:ui-value      (v t))
(define-struct sham:exp:void-value    ())
(define-struct sham:exp:sizeof        (t))
(define-struct sham:exp:type          (t))
(define-struct sham:exp:gep           (ptr indxs))
(define-struct sham:exp:var           (v))

(define-struct sham:rator:intrinsic   (str-id ret-type))
(define-struct sham:rator:symbol      (sym))

;; (define (print-ast ast)
;;   (define as print-ast)
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
;;      ;; `(let ((,(as id) : ,id-type ,(as id-val)))
;;      ;;    ,(as body))
;;      `(,(as id) ,@(as body))
;;      ]
;;     [(ast-stmt-set! lhs val)
;;      `(set! ,(as lhs) ,(as val))]
;;     [(ast-stmt-store! lhs val)
;;      `(store! ,(as lhs) ,(as val))]
;;     [(ast-stmt-if tst thn els)
;;       `(if ,(as tst) ,(as thn) ,(as els))]
;;     [(ast-stmt-while phi-vars phi-types test body)
;;      `(while (phi ,@(for/list ([p phi-vars]
;;                                [t phi-types])
;;                       `(,(as p) : ,t)))
;;         ,(as test) ,(as body))]
;;     [(ast-stmt-ret val)
;;      `(return ,(as val))]
;;     [(ast-stmt-ret-void)
;;      `(return-void)]
;;     [(ast-stmt-block bodys)
;;      (map as bodys)]
;;     [(ast-stmt-exp val)
;;      (as val)]

;;     [(ast-exp-app rator rands)
;;      `(,(as rator) ,@(map as rands))]
;;     [(ast-exp-fl-value v t)
;;      `(float ,(as v) ,(as t))]
;;     [(ast-exp-si-value v t)
;;      `(sint ,(as v) ,(as t))]
;;     [(ast-exp-ui-value v t)
;;      `(uint ,(as v) ,(as t))]
;;     [(ast-exp-void-value)
;;      '<void>]
;;     [(ast-exp-sizeof t)
;;      `(sizeof ,(as t))]
;;     [(ast-exp-type t)
;;      `(type ,t)]
;;     [(ast-exp-gep ptr indxs)
;;      `(gep ,(as ptr) ,(map as indxs))]
;;     [(ast-exp-var v)
;;      v]
;;     [else ast]))

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

