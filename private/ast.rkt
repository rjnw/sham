#lang racket

(provide (all-defined-out))

(define-struct ast-function-def  (id passes attrs arg-ids arg-types ret-type body))
(define-struct ast-type-def      (id type))

(define-struct ast-type-ref       (to))
(define-struct ast-type-struct    (fields types))
(define-struct ast-type-function  (args ret))
(define-struct ast-type-pointer   (to))

(define-struct ast-stmt-let       (ids id-types id-vals body))
(define-struct ast-stmt-set!      (lhs val))
(define-struct ast-stmt-store!    (lhs val))
(define-struct ast-stmt-if        (test then else))
(define-struct ast-stmt-while     (phi-vars phi-types test body))
(define-struct ast-stmt-ret       (val))
(define-struct ast-stmt-ret-void  ())
(define-struct ast-stmt-blocks    (bodys))
(define-struct ast-stmt-exp       (val))

(define-struct ast-exp-app        (rator rands))
(define-struct ast-exp-fl-value   (v t))
(define-struct ast-exp-si-value   (v t))
(define-struct ast-exp-ui-value   (v t))
(define-struct ast-exp-sizeof     (t))
(define-struct ast-exp-type       (t))
(define-struct ast-exp-gep        (ptr indxs))
(define-struct ast-exp-var        (v))


(define (print-ast ast)
  (define as ast->sexp)
  (match ast
    [(ast-function-def id passes attrs arg-ids arg-types ret-type body)
     `(define-function (#:pass ,@passes) (#:attr ,@attrs)
        (,id ,@(for/list ([i arg-ids]
                          [t arg-types])
                 `(,i : ,t)) : ,ret-type)
        ,(as body))]
    [(ast-type-def id type)
     `(define-type ,id ,(as type))]

    [(ast-type-ref to)
     to]
    [(ast-type-struct fields types)
     `(struct ,@(for/list ([i fields] [t types]) `(,i : ,t)))]
    [(ast-type-function args ret)
     `(,@args -> ,ret)]
    [(ast-type-pointer to)
     `(pointer ,to)]

    [(ast-stmt-let ids id-types id-vals body)
     `(let ,(for/list ([i ids]
                       [t id-types]
                       [v id-vals])
              `(,i ,t ,(as v)))
        ,(as body))]
    [(ast-stmt-set! lhs val)
     `(set! ,lhs ,(as val))]
    [(ast-stmt-store! lhs val)
     `(store! ,lhs ,(as val))]
    [(ast-stmt-if tst thn els)
      `(if ,(as tst) ,(as thn) ,(as els))]
    [(ast-stmt-while phi-vars phi-types test body)
     `(while (phi ,@(for/list ([p phi-vars]
                               [t phi-types])
                      `(,p : ,t)))
        ,(as test) ,(as body))]
    [(ast-stmt-ret val)
     `(return ,(as val))]
    [(ast-stmt-ret-void)
     `(return-void)]
    [(ast-stmt-blocks bodys)
     `(begin ,@(map as bodys))]
    [(ast-stmt-exp val)
     (as val)]

    [(ast-exp-app rator rands)
     `(,(as rator) ,@(map as rands))]
    [(ast-exp-fl-value v t)
     `(float ,(as v) ,(as t))]
    [(ast-exp-si-value v t)
     `(sint ,(as v) ,(as t))]
    [(ast-exp-ui-value v t)
     `(uint ,(as v) ,(as t))]
    [(ast-exp-sizeof t)
     `(sizeof ,(as t))]
    [(ast-exp-type t)
     `(type ,t)]
    [(ast-exp-gep ptr indxs)
     `(gep ,(as ptr) ,(map as indxs))]
    [(ast-exp-var v)
     v]
    [else ast]))

(define (ast->sexp ast)
  (define as ast->sexp)
  (match ast
    [(ast-function-def id passes attrs arg-ids arg-types ret-type body)
     `(define-function (#:pass ,@passes) (#:attr ,@attrs)
        (,id ,@(for/list ([i arg-ids]
                          [t arg-types])
                 `(,i : ,t)) : ,ret-type)
        ,(as body))]
    [(ast-type-def id type)
     `(define-type ,id ,(as type))]

    [(ast-type-ref to)
     to]
    [(ast-type-struct fields types)
     `(struct ,@(for/list ([i fields] [t types]) `(,i : ,t)))]
    [(ast-type-function args ret)
     `(,@args -> ,ret)]
    [(ast-type-pointer to)
     `(pointer ,to)]

    [(ast-stmt-let ids id-types id-vals body)
     `(let ,(for/list ([i ids]
                       [t id-types]
                       [v id-vals])
              `(,i : ,t ,(as v)))
        ,(as body))]
    [(ast-stmt-set! lhs val)
     `(set! ,lhs ,(as val))]
    [(ast-stmt-store! lhs val)
     `(store! ,lhs ,(as val))]
    [(ast-stmt-if tst thn els)
      `(if ,(as tst) ,(as thn) ,(as els))]
    [(ast-stmt-while phi-vars phi-types test body)
     `(while ,(for/list ([p phi-vars]
                         [t phi-types])
                `(,p : ,t))
        ,(as test) ,(as body))]
    [(ast-stmt-ret val)
     `(return ,(as val))]
    [(ast-stmt-ret-void)
     `(return-void)]
    [(ast-stmt-blocks bodys)
     `(begin ,@(map as bodys))]
    [(ast-stmt-exp val)
     `(#%exp ,(as val))]

    [(ast-exp-app rator rands)
     `(#%app ,(as rator) ,@(map as rands))]
    [(ast-exp-fl-value v t)
     `(#%fl-value ,(as v) ,(as t))]
    [(ast-exp-si-value v t)
     `(#%si-value ,(as v) ,(as t))]
    [(ast-exp-ui-value v t)
     `(#%ui-value ,(as v) ,(as t))]
    [(ast-exp-sizeof t)
     `(#%sizeof ,(as t))]
    [(ast-exp-type t)
     `(#%type ,t)]
    [(ast-exp-gep ptr indxs)
     `(#%gep ,(as ptr) ,(map as indxs))]
    [(ast-exp-var v)
     v]
    [else ast]))

(define (sexp->ast s)
  (define sa sexp->ast)
  (match s
    [`(define-function (#:pass ,passes ...) (#:attr ,attrs ...)
        (,id (,arg-ids : ,arg-types) ... : ,ret-type)
        ,body)
     (ast-function-def id passes attrs arg-ids arg-types ret-type body)]
    [`(define-type ,id (struct (,fields : ,types)))
     (ast-type-def id (ast-type-struct fields types))]
    [`(define-type ,id (,args ... -> ,ret))
     (ast-type-def id (ast-type-function args ret))]
    [`(define-type ,id (pointer ,to))
     (ast-type-def id (ast-type-pointer to))]
    [`(define-type ,id ,to) #:when (symbol? to)
     (ast-type-def (ast-type-ref to))]
    [`(let ((,ids : ,types ,vals) ...) ,st)
     (ast-stmt-let ids types (map sa vals) (sa st))]
    [`(set! ,lhs ,v)
     (ast-stmt-set! lhs (sa v))]
    [`(store! ,lhs ,v)
     (ast-stmt-store! lhs (sa v))]
    [`(if ,tst ,thn ,els)
     (ast-stmt-if (sa tst) (sa thn) (sa els))]
    [`(while ((,vars : ,var-types) ...) ,tst ,body)
     (ast-stmt-while vars var-types (sa tst) (sa body))]
    [`(return ,v)
     (ast-stmt-ret (sa v))]
    [`(return-void)
     (ast-stmt-ret-void)]
    [`(block ,stmts ...)
     (ast-stmt-blocks (map sa stmts))]
    [`(#%exp ,e)
     (ast-stmt-exp (sa e))]
    [`(#%app ,rator ,rands ...)
     (ast-exp-app (sa rator) (map sa rands))]
    [`(#%fl-value ,value ,type)
     (ast-exp-fl-value value (sa type))]
    [`(#%si-value ,value ,type)
     (ast-exp-si-value value (sa type))]
    [`(#%ui-value ,value ,type)
     (ast-exp-ui-value value (sa type))]
    [`(#%sizeof ,type)
     (ast-exp-sizeof (sa type))]
    [`(#%type ,t)
     (ast-exp-type t)]
    [`(#%gep ,ptr ,indxs)
     (ast-exp-gep (sa ptr) (map sa indxs))]
    [(? symbol?)
     (ast-exp-var s)]))

;sample match
#;(match ast
    [(ast-function-def id args ret-type body)]
    [(ast-type-ref to)]
    [(ast-type-struct ids types)]
    [(ast-type-function args ret)]
    [(ast-type-pointer to)]
    [(ast-stmt-let ids id-types id-vals body)]
    [(ast-stmt-set! lhs val)]
    [(ast-stmt-store! lhs val)]
    [(ast-stmt-if tst thn els)]
    [(ast-stmt-while phi-vars phi-types test body)]
    [(ast-stmt-ret val)]
    [(ast-stmt-ret-void)]
    [(ast-stmt-blocks bodys)]
    [(ast-stmt-exp val)]
    [(ast-exp-app rator rands)]
    [(ast-exp-fl-value v t)]
    [(ast-exp-si-value v t)]
    [(ast-exp-ui-value v t)]
    [(ast-exp-sizeof t)]
    [(ast-exp-type t)]
    [(ast-exp-gep ptr indxs)]
    [(ast-exp-var v)])
