#lang racket

(provide (all-defined-out))

(struct sham:module ([info #:mutable] defs))


(struct sham:def    ([info #:mutable] id))

(struct sham:def:function sham:def (arg-ids arg-types ret-type body))
(struct sham:def:type     sham:def (type))
(struct sham:def:global   sham:def (type))

(struct sham:ast ([metadata #:auto #:mutable]) #:auto-value (void))


(struct sham:type sham:ast ())

(struct sham:type:internal sham:type ())
(struct sham:type:ref      sham:type (to))
(struct sham:type:struct   sham:type (fields types))
(struct sham:type:function sham:type (args ret))
(struct sham:type:pointer  sham:type (to))


(struct sham:stmt sham:ast ())


(struct sham:stmt:set!   sham:stmt (lhs val))
(struct sham:stmt:if     sham:stmt (tst thn els))
(struct sham:stmt:while  sham:stmt (tst body))
(struct sham:stmt:return sham:stmt (val))
(struct sham:stmt:void   sham:stmt ())
(struct sham:stmt:expr   sham:stmt (e))
(struct sham:stmt:block  sham:stmt (stmts))


(struct sham:expr sham:ast ())

(struct sham:expr:app      sham:expr (rator rands))
(struct sham:expr:fl-value sham:expr (v t))
(struct sham:expr:si-value sham:expr (v t))
(struct sham:expr:ui-value sham:expr (v t))
(struct sham:expr:void     sham:expr ())
(struct sham:expr:sizeof   sham:expr (t))
(struct sham:expr:type     sham:expr (t)) ;;only used for malloc and variants
(struct sham:expr:gep      sham:expr (ptr indxs))
(struct sham:expr:var      sham:expr (id))
(struct sham:expr:global   sham:expr (id))
(struct sham:expr:external sham:expr (lib-id id t))
(struct sham:expr:let      sham:expr (ids id-types id-vals stmt expr))


(struct sham:rator ())

(struct sham:rator:intrinsic sham:rator (str-id ret-type))
(struct sham:rator:symbol    sham:rator (sym))
(struct sham:rator:external  sham:rator (lib-id str-id ret-type))

;; ;;TODO add debug parameters to printer
(define (print-sham-type t)
  (match t
    [(sham:type:internal md) '_]
    [(sham:type:ref md to) to]
    [(sham:type:struct md fields types)
     `(struct ,(for/list [(f fields) (t types)] `(,f ,(print-sham-type t))))]
    [(sham:type:function md args ret)
     `(,@(map print-sham-type args) ,(print-sham-type ret))]
    [(sham:type:pointer md to) `(* ,(print-sham-type to))]))

(define (print-sham-stmt stmt)
  (match stmt
    [(sham:stmt:expr md e) (list (print-sham-expr e))]
    [(sham:stmt:set! md lhs val)
     `((set! ,(print-sham-expr lhs) ,(print-sham-expr val)))]
    [(sham:stmt:if md tst thn els)
     `((if ,(print-sham-expr tst) ,@(print-sham-stmt thn) ,@(print-sham-stmt els)))]
    [(sham:stmt:while md tst body)
     `((while ,(print-sham-expr tst) ,@(print-sham-stmt body)))]
    [(sham:stmt:return md val) `((return ,(print-sham-expr val)))]
    [(sham:stmt:expr md l)
     #:when (sham:expr:let? l)
     (print-sham-expr l)]
    [(sham:stmt:block md stmts) (append-map print-sham-stmt stmts)]
    [(sham:stmt:void md) `(svoid)]))

(define (print-sham-expr e)
  (match e
    [(sham:expr:let md ids ts vs st e)
     #:when (sham:expr:void? e)
     `(let ,@(map (λ (v) `(,(car v)
                           ,(print-sham-expr (cdr v))))
                  (map cons ids vs))
        ,@(print-sham-stmt st))]
    [(sham:expr:let md ids ts vs st e)
     `(let ,@(map (λ (v) `(,(car v)
                           ,(print-sham-expr (cdr v))))
                  (map cons ids vs))
        ,@(print-sham-stmt st)
        ,(print-sham-expr e))]
    [(sham:expr:app md rator rands)
     `(,(print-sham-rator rator) ,@(map print-sham-expr rands))]
    [(sham:expr:fl-value md v t) `(float ,v ,(print-sham-type t))]
    [(sham:expr:si-value md v t) `(sint ,v ,(print-sham-type t))]
    [(sham:expr:ui-value md v t) `(uint ,v ,(print-sham-type t))]
    [(sham:expr:void md) `evoid]
    [(sham:expr:sizeof md t) `(sizeof ,(print-sham-type t))]
    [(sham:expr:type md t) `(%type ,(print-sham-type t))]
    [(sham:expr:global md id) `(global ,id)]
    [(sham:expr:external md lib-id id t) `(external ,id ,lib-id)]
    [(sham:expr:gep md ptr indxs)
     `(gep ,(print-sham-expr ptr) ,@(map print-sham-expr indxs))]
    [(sham:expr:var md v) v]))

(define (print-sham-rator r)
  (match r
    [(sham:rator:intrinsic str-id ret-type)
     str-id]
    [(sham:rator:symbol s)
     s]
    [(sham:rator:external lib-id str-id ret-type)
     `(external ,lib-id ,str-id)]))

(define (print-sham-def def)
  (match def
    [(sham:def:function info id arg-ids arg-types ret-type body)
     `(define (,id ,@arg-ids)
        ,@(print-sham-stmt body))]
    [(sham:def:type info id t)
     `(define ,id ,(print-sham-type t))]
    [(sham:def:global info id t)
     `(global ,id ,(print-sham-type t))]))

(define (print-sham-module mod)
  (match mod
    [(sham:module info defs)
     `(module
          ,@(map print-sham-def defs))]))

;; (define (sham-type->sexp t)
;;   (match t
;;     [(sham:type:internal)
;;      '_]
;;     [(sham:type:ref to)
;;      to]
;;     [(sham:type:struct fields types)
;;      `(struct ,(for/list [(f fields) (t types)] `(,f : ,(sham-type->sexp t))))]
;;     [(sham:type:function args ret)
;;      `(,@(map sham-type->sexp args) -> ,(sham-type->sexp ret))]
;;     [(sham:type:pointer to)
;;      `(* ,(sham-type->sexp to))]))

;; (define (sham-stmt->sexp stmt)
;;   (match stmt
;;     [(sham:stmt:set! lhs val)
;;      `(set! ,(sham-expr->sexp lhs) ,(sham-expr->sexp val))]
;;     [(sham:stmt:if tst thn els)
;;      `(if ,(sham-expr->sexp tst) ,(sham-stmt->sexp thn) ,(sham-stmt->sexp els))]
;;     [(sham:stmt:while tst body)
;;      `(while ,(sham-expr->sexp tst) ,(sham-stmt->sexp body))]
;;     [(sham:stmt:return val)
;;      `(return ,(sham-expr->sexp val))]
;;     [(sham:stmt:void)
;;      'svoid]
;;     [(sham:stmt:expr e)
;;      `(expr ,(sham-expr->sexp e))]
;;     [(sham:stmt:block stmts)
;;      `(block ,@(map sham-stmt->sexp stmts))]))

;; (define (sham-expr->sexp e)
;;   (match e
;;     [(sham:expr:let ids ts vs st e)
;;      `(let ,(for/list [(i ids) (t ts) (v vs)]
;;               `(,i : ,(sham-type->sexp t) ,(sham-expr->sexp v)))
;;         ,(sham-stmt->sexp st)
;;         ,(sham-expr->sexp e))]
;;     [(sham:expr:app rator rands)
;;      `(,(sham-rator->sexp rator) ,@(map sham-expr->sexp rands))]
;;     [(sham:expr:fl-value v t)
;;      `(%float ,v ,(sham-type->sexp t))]
;;     [(sham:expr:si-value v t)
;;      `(%sint ,v ,(sham-type->sexp t))]
;;     [(sham:expr:ui-value v t)
;;      `(%uint ,v ,(sham-type->sexp t))]
;;     [(sham:expr:void)
;;      'evoid]
;;     [(sham:expr:sizeof t)
;;      `(%sizeof ,(sham-type->sexp t))]
;;     [(sham:expr:type t)
;;      `(%type ,(sham-type->sexp t))]
;;     [(sham:expr:global id)
;;      `(%global ,id)]
;;     [(sham:expr:external lib-id id t)
;;      `(%external ,lib-id ,id ,(sham-type->sexp t))]
;;     [(sham:expr:gep ptr indxs)
;;      `(%gep ,(sham-expr->sexp ptr) ,@(map sham-expr->sexp indxs))]
;;     [(sham:expr:var v)
;;      v]))

;; (define (sham-rator->sexp r)
;;   (match r
;;     [(sham:rator:intrinsic str-id ret-type)
;;      `(intrinsic ,str-id ,(sham-type->sexp ret-type))]
;;     [(sham:rator:external lib-id str-id ret-type)
;;      `(external ,lib-id ,str-id ,(sham-type->sexp ret-type))]
;;     [(sham:rator:symbol s) s]))

;; (define (sham-def->sexp def)
;;   (match def
;;     [(sham:def:function id info arg-ids arg-types ret-type body)
;;      `(define-function ,info
;;         (,id ,@(for/list [(id arg-ids) (typ arg-types)]
;;                  `(,id : ,(sham-type->sexp typ)))
;;              : ,(sham-type->sexp ret-type))
;;         ,(sham-stmt->sexp body))]
;;     [(sham:def:type id t)
;;      `(define-type ,id ,(sham-type->sexp t))]
;;     [(sham:def:global id type)
;;      `(define-global ,id ,(sham-type->sexp type))]))

;; (define (sham-ast->sexp ast)
;;   (match ast
;;     [(sham:module info defs)
;;      `(module
;;           ,info
;;           ,@(map sham-def->sexp defs))]))

;; ;;;---;;;
;; (define (sexp->sham-ast sexp)
;;   (match sexp
;;     [`(module
;;           ,info
;;           ,defs ...)
;;      (sham:module info (map sexp->sham-def defs))]))

;; (define (sexp->sham-type t)
;;   (match t
;;     ['_
;;      (sham:type:internal)]
;;     [`(* ,to)
;;      (sham:type:pointer (sexp->sham-type to))]
;;     [`(,args ... -> ,ret)
;;      (sham:type:function (map sexp->sham-type args) (sexp->sham-type ret))]
;;     [`(struct ((,fields : ,types) ...))
;;      (sham:type:struct fields (map sexp->sham-type types))]
;;     [to #:when (symbol? to)
;;      (sham:type:ref to)]))

;; (define (sexp->sham-stmt sexp)
;;   (match sexp
;;     [`(set! ,lhs ,val)
;;      (sham:stmt:set! (sexp->sham-expr lhs) (sexp->sham-expr val))]
;;     [`(if ,tst ,thn ,els)
;;      (sham:stmt:if (sexp->sham-expr tst)
;;                    (sexp->sham-stmt thn)
;;                    (sexp->sham-stmt els))]
;;     [`(while ,tst ,body)
;;      (sham:stmt:while (sexp->sham-expr tst) (sexp->sham-stmt body))]
;;     [`(return ,val)
;;      (sham:stmt:return (sexp->sham-expr val))]
;;     [`(block ,stmts ...)
;;      (sham:stmt:block (map sexp->sham-stmt stmts))]
;;     ['svoid (sham:stmt:void)]))

;; (define (sexp->sham-expr e)
;;   (match e
;;     [`(let ((,ids : ,types ,vals) ...) ,stmt ,expr)
;;      (sham:stmt:let ids (map sexp->sham-type types) (map sexp->sham-expr vals)
;;                     (sexp->sham-stmt stmt)
;;                     (sexp->sham-expr expr))]

;;     [`(%float ,v ,t)
;;      (sham:expr:fl-value v (sexp->sham-type t))]
;;     [`(%sint ,v ,t)
;;      (sham:expr:si-value v (sexp->sham-type t))]
;;     [`(%uint ,v ,t)
;;      (sham:expr:ui-value v (sexp->sham-type t))]
;;     ['evoid
;;      (sham:expr:void)]
;;     [`(%sizeof ,t)
;;      (sham:expr:sizeof (sexp->sham-type t))]
;;     [`(%type ,t)
;;      (sham:expr:type (sexp->sham-type t))]
;;     [`(%gep ,ptr ,indxs ...)
;;      (sham:expr:gep (sexp->sham-expr ptr) (map sexp->sham-expr indxs))]
;;     [`(,rator ,rands ...)
;;      (sham:expr:app (sexp->sham-rator rator) (map sexp->sham-expr rands))]
;;     [v #:when (symbol? v)
;;        (sham:expr:var v)]))

;; (define (sexp->sham-rator r)
;;   (match r
;;     [`(intrinsic ,str-id ,ret-type)
;;      (sham:rator:intrinsic str-id (sexp->sham-type ret-type))]
;;     [`(external ,lib-id ,str-id ,ret-type)
;;      (sham:rator:external lib-id str-id (sexp->sham-type ret-type))]
;;     [s #:when (symbol? s)
;;        (sham:rator:symbol s)]))

;; (define (sexp->sham-def def)
;;   (match def
;;     [`(define-function (passes ,passes ...)
;;         (attrs ,attrs ...)
;;         (,id (,arg-ids : ,arg-types) ... : ,ret-type)
;;         ,body)
;;      (sham:def:function id passes attrs arg-ids (map sexp->sham-type arg-types)
;;                         (sexp->sham-type  ret-type)
;;                         (sexp->sham-stmt body))]
;;     [`(define-type ,id ,t)
;;      (sham:def:type id (sexp->sham-type t))]))


;; ;;utils
(module* utils #f
  (provide (except-out (all-defined-out)
                       define-bind-sym-checker
                       check-sym
                       check-expr
                       is-list-stmt?
                       check-list-stmt
                       check-stmt
                       check-type))

  (define (basic-function-info) (void))
  (define (basic-module-info) (void))
  (define (basic-type-info) (void))

  (define-syntax-rule (sham$def:type id t)
    (sham:def:type (basic-type-info) (check-sym id) t))

  (define (sham:stmt:let ids id-types id-vals stmt);backward compatibility, or short key as well
    (sham:stmt:expr (sham:expr:let ids id-types id-vals stmt (sham:expr:void))))
  (define sham$slet sham:stmt:let)

  ;simple shorts
  (define sham$set! sham:stmt:set!)
  (define sham$if sham:stmt:if)
  (define sham$while sham:stmt:while)
  (define sham$return sham:stmt:return)
  (define sham$svoid (sham:stmt:void))
  (define sham$stexpr sham:stmt:expr)

  (define sham$flv sham:expr:fl-value)
  (define sham$siv sham:expr:si-value)
  (define sham$uiv sham:expr:ui-value)
  (define sham$evoid (sham:expr:void))
  (define sham$sizeof sham:expr:sizeof)
  (define-syntax-rule (sham$etype t)
    (sham:expr:type (sham$tref t)))
  (define sham$gep sham:expr:gep)
  (define sham$global sham:expr:global) ;; maybe copy the var macro here as well
  (define sham$external sham:expr:external)
  (define sham$let sham:expr:let)

  (define sham$ri sham:rator:intrinsic)

  (define sham$re sham:rator:external)

  (require (for-syntax syntax/parse))

  ; a little complicated shorts, but should make my life easier
  (define-syntax-rule (define-bind-sym-checker mid astf astf?)
    (define-syntax (mid stx)
      (define (check-if-symbol e)
        #`(cond
           [(symbol? #,e) (astf #,e)]
           [(astf? #,e) #,e] ;this will allow (sham$rs (sham$rs (... 'a))), don't know if we want that?
           [else (error "invalid input for: " (object-name astf) #,e)]))
      (syntax-parse stx
        [(_ i:id)
         (if (identifier-binding #'i) (check-if-symbol #'i) #'(astf 'i))]
        [(_ e:expr) (check-if-symbol #'e)])))

  (define-bind-sym-checker sham$rator sham:rator:symbol sham:rator?)
  (define-bind-sym-checker sham$rs sham:rator:symbol sham:rator:symbol?)
  (define-bind-sym-checker sham$var sham:expr:var sham:expr:var?)

  (define-bind-sym-checker sham$tref sham:type:ref sham:type?)
  (define-bind-sym-checker check-sym identity symbol?)
  (define-bind-sym-checker check-expr sham:expr:var sham:expr?)

  (define  (is-list-stmt? l) (if (andmap sham:stmt? l) l (error "not a list of statements")))
  (define-bind-sym-checker check-list-stmt is-list-stmt? is-list-stmt?)

  (define-bind-sym-checker check-stmt (const (error "not a statement")) sham:stmt?)
  (define-bind-sym-checker check-type (const (error "not a type")) sham:type?)

  (define-syntax (sham$app stx) ;;TODO move the rator parsing to sham$rator
    (syntax-parse stx
      [(_ (i:id t:expr) rands:expr ...)
       #'(sham:expr:app (sham$ri (check-sym i) (sham$tref t))
                        (list (check-expr rands) ...))]
      [(_ (l:id i:id t:id) rands:expr ...)
       #'(sham:expr:app (sham$re (check-sym l) (check-sym i) (sham$tref t))
                        (list (check-expr rands) ...))]
      [(_ rator:expr rands:expr ...)
       #'(sham:expr:app (sham$rator 'rator) (list (check-expr rands) ...))]))
  ;;forcing rator to be sym, otherwise messes up with load, maybe we can check for all the intrinsic names :P

  (define-syntax (sham$block stx)
    (syntax-case stx ()
      [(_ b) #'(sham:stmt:block (check-list-stmt b))]
      [(_ s ...) #'(sham:stmt:block (list (check-stmt s) ...))]))

  (define-syntax (sham$define stx)
    (syntax-parse stx
      [(_ (name:id (args:id t:expr) ... rett:id) stmt:expr)
       #'(sham:def:function
          (basic-function-info) (check-sym name)
          (list (check-sym args) ...) (list (sham$tref t) ...) (sham$tref rett)
          (check-stmt stmt))]
      [(_ (name:id info:expr (args:id t:expr) ... rett:id) stmt:expr)
       #'(sham:def:function
          info (check-sym name)
          (list (check-sym args) ...) (list (sham$tref t) ...) (sham$tref rett)
          (check-stmt stmt))]
      [(_ (name:id info:expr rett:id) stmt:expr)
       #'(sham:def:function
          info (check-sym name)
          '() '() (sham$tref rett)
          (check-stmt stmt))]
      [(_ (name:id rett:id) stmt:expr)
       #'(sham:def:function
          (basic-function-info)
          (check-sym name)
          '() '() (sham$tref rett)
          (check-stmt stmt))])))
