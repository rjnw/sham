#lang racket

(require "ast.rkt"
         "parameters.rkt")

(provide (all-defined-out))

(define i1  (sham:ast:type:ref 'i1))
(define i8  (sham:ast:type:ref 'i8))
(define i16  (sham:ast:type:ref 'i16))
(define i32 (sham:ast:type:ref 'i32))
(define i64 (sham:ast:type:ref 'i64))

(define i8* (sham:ast:type:pointer i8))
(define i16* (sham:ast:type:pointer i16))
(define i32* (sham:ast:type:pointer i32))
(define i64* (sham:ast:type:pointer i64))

(define ui sham:ast:expr:const:ui)
(define (ui1 v) (sham:ast:expr:const:ui v i1))
(define (ui8 v) (sham:ast:expr:const:ui v i8))
(define (ui32 v) (sham:ast:expr:const:ui v i32))
(define (ui64 v) (sham:ast:expr:const:ui v i64))

(define si sham:ast:expr:const:si)
(define (si32 v) (sham:ast:expr:const:si v i32))
(define (si64 v) (sham:ast:expr:const:si v i64))

(define f32 (sham:ast:type:ref 'f32))
(define f64 (sham:ast:type:ref 'f64))
(define f32* (sham:ast:type:pointer f32))
(define f64* (sham:ast:type:pointer f64))

(define fl sham:ast:expr:const:fl)
(define (fl32 v) (sham:ast:expr:const:fl v f32))
(define (fl64 v) (sham:ast:expr:const:fl v f64))

(define tvoid (sham:ast:type:ref 'void))

(define dmodule sham:def:module)
(define dfunction sham:def:function)
(define dtype sham:def:type)
(define dglobal sham:def:global)
(define dg-string sham:def:global-string)

(define tref sham:ast:type:ref)
(define tstruct sham:ast:type:struct)
(define tfun sham:ast:type:function)
(define tptr sham:ast:type:pointer)
(define tarr sham:ast:type:array)
(define tvec sham:ast:type:vector)

(define v4i32 (tvec i32 4))
(define v4i64 (tvec i64 4))
(define v4f32 (tvec f32 4))
(define v4f64 (tvec f64 4))

(define set!^ sham:ast:stmt:set!)
(define if^ sham:ast:stmt:if)
(define switch sham:ast:stmt:switch)
(define break sham:ast:stmt:break)
(define while sham:ast:stmt:while)
(define return sham:ast:stmt:return)
(define svoid sham:ast:stmt:void)
(define se sham:ast:stmt:expr)

(define app sham:ast:expr:app)
(define evoid sham:ast:expr:void)
(define sizeof sham:ast:expr:sizeof)
(define etype sham:ast:expr:etype)
(define gep sham:ast:expr:gep)
(define var sham:ast:expr:var)
(define v sham:ast:expr:var)
(define global sham:ast:expr:global)
(define external sham:ast:expr:external)
;; (define let sham:ast:expr:let) use let^

(define rs sham:ast:rator:symbol)

(define re sham:ast:rator:external)
(define rr sham:ast:rator:racket)

(define cfl sham:ast:expr:const:fl)
(define csi sham:ast:expr:const:si)
(define cui sham:ast:expr:const:ui)
(define cstring sham:ast:expr:const:string)
(define cllvm sham:ast:expr:const:llvm)
(define cstruct sham:ast:expr:const:struct)
(define carray sham:ast:expr:const:array)
(define cvector sham:ast:expr:const:vector)
(define (vec . vals) (sham:ast:expr:const:vector vals))

(define (return-void) (sham:ast:stmt:return (sham:ast:expr:void)))
(define (ret v) (sham:ast:stmt:return v))
(define (ret-void) (sham:ast:stmt:return (sham:ast:expr:void)))
(define (gep^ ptr . indexes) (gep ptr indexes))

;; internal function
(define (irs sym)
  (λ args (app (rs sym) args)))

(define icmp-eq (irs 'icmp-eq))
(define icmp-ne (irs 'icmp-ne))
(define icmp-ugt (irs 'icmp-ugt))
(define icmp-uge (irs 'icmp-uge))
(define icmp-ult (irs 'icmp-ult))
(define icmp-ule (irs 'icmp-ule))
(define icmp-sgt (irs 'icmp-sgt))
(define icmp-sge (irs 'icmp-sge))
(define icmp-slt (irs 'icmp-slt))
(define icmp-sle (irs 'icmp-sle))

(define fcmp-oeq (irs 'fcmp-oeq))
(define fcmp-ogt (irs 'fcmp-ogt))
(define fcmp-oge (irs 'fcmp-oge))
(define fcmp-olt (irs 'fcmp-olt))
(define fcmp-ole (irs 'fcmp-ole))
(define fcmp-one (irs 'fcmp-one))
(define fcmp-ord (irs 'fcmp-ord))
(define fcmp-uno (irs 'fcmp-uno))
(define fcmp-ueq (irs 'fcmp-ueq))
(define fcmp-ugt (irs 'fcmp-ugt))
(define fcmp-uge (irs 'fcmp-uge))
(define fcmp-ult (irs 'fcmp-ult))
(define fcmp-ule (irs 'fcmp-ule))
(define fcmp-une (irs 'fcmp-une))

(define add (irs 'add))
(define add-nsw (irs 'add-nsw))
(define add-nuw (irs 'add-nuw))
(define fadd (irs 'fadd))

(define sub (irs 'sub))
(define sub-nsw (irs 'sub-nsw))
(define sub-nuw (irs 'sub-nuw))
(define fsub (irs 'fsub))

(define mul (irs 'mul))
(define mul-nsw (irs 'mul-nsw))
(define mul-nuw (irs 'mul-nuw))
(define fmul (irs 'fmul))

(define udiv (irs 'udiv))
(define sdiv (irs 'sdiv))
(define exact-sdiv (irs 'exact-sdiv))
(define fdiv (irs 'fdiv))

(define urem (irs 'urem))
(define srem (irs 'srem))
(define frem (irs 'frem))

(define shl (irs 'shl))
(define lshr (irs 'lshr))
(define ashr (irs 'ashr))

(define or^ (irs 'or))
(define xor^ (irs 'xor))
(define and^ (irs 'and))
(define not^ (irs 'not))

(define malloc^ (irs 'malloc))
(define free^ (irs 'free))
(define arr-malloc (irs 'arr-malloc))
(define arr-alloca (irs 'arr-alloca))

;;casts
(define trunc  (irs 'trunc))
(define zext   (irs 'zext))
(define sext   (irs 'sext))
(define fp->ui (irs 'fp->ui))
(define fp->si (irs 'fp->si))
(define ui->fp (irs 'ui->fp))
(define si->fp (irs 'si->fp))
(define fp-trunc (irs 'fp-trunc))
(define fp-ext   (irs 'fp-ext))
(define ptr->int (irs 'ptr->int))
(define int->ptr (irs 'int->ptr))
(define bitcast  (irs 'bitcast))
(define addrspacecast (irs 'addrspacecast))
(define zextorbitcast (irs 'zextorbitcast))
(define sextorbitcast (irs 'sextorbitcast))
(define ptrcast  (irs 'ptrcast))
(define intcast  (irs 'intcast))
(define fpcast   (irs 'fpcast))

(define load (irs 'load))
(define store! (irs 'store!))


(define (while^ expr . stmts)
  (while expr (block stmts)))
(define (while-ule^ ind bound . stmts)
  (while (icmp-ule ind bound) (block stmts)))

;; intrinsics
(define (intrinsic . args)
  (define sargs (map symbol->string args))
  (define s. (string-join sargs "."))
  (string->symbol (format "llvm.~a" s.)))

(require (for-syntax racket/syntax syntax/parse racket/pretty))
(define-syntax (li stx)
  (syntax-parse stx
    [(_ names:id ...)
     (define sl (for/list ([name (syntax->list #`(names ...))])
                  (define fname (datum->syntax name (string->symbol (format "ri-~a" (syntax->datum name)))))
                  #`(define-syntax-rule
                      (#,fname iarg ret-type args #,'...)
                      (app (ri (intrinsic (quote #,name) (quote iarg)) ret-type) (list args #,'...)))))
     ;; (pretty-print (map syntax->datum sl))
     #`(begin #,@sl)]))

(li memcpy memmove memset sqrt powi sin cos pow exp exp2 log log10 log2 fma fabs
    minnum maxnum copysign floor ceil trunc rint nearbyint round bitreverse
    bswap ctpop ctlz cttz fshl fshr sadd.with.overflow uadd.with.overflow
    ssub.with.overflow usub.with.overflow smul.with.overflow umul.with.overflow
    canonicalize fmuladd)

(define ri sham:ast:rator:intrinsic)
(define-syntax-rule (ri^ intr ret-type args ...)
  (app^ (ri (intrinsic (quote intr)) ret-type) args ...))
(define-syntax (let^ stx)
  (syntax-parse stx
    [(_ ([arg val (~datum :) typ] ...) s:expr ... e:expr)
     #`(let ([arg (v (quasiquote arg))] ...)
         (sham:ast:expr:let (list (quasiquote arg) ...) (list typ ...) (list val ...)
                            (block^ s ...)
                            e))]))
(define-syntax (slet^ stx)
  (syntax-parse stx
    [(_ ([arg val (~datum :) typ] ...) s:expr ...)
     #`(let ([arg (v (quasiquote arg))] ...)
         (sham:ast:expr:let (list (quasiquote arg) ...) (list typ ...) (list val ...)
                            (block^ s ...)
                            (evoid)))]))
(define-syntax (switch^ stx)
  (syntax-parse stx
    [(_ v:expr [case:expr branch:expr] ... default)
     #`(switch v (list case ...) (list branch ...) default)]))

(define (block stmts)
  (sham:ast:stmt:block
   (map
    (λ (v) (cond [(sham:ast:expr? v) (se v)]
                 [(sham:ast:stmt? v) v]
                 [else (error "block expects a stmt/expr given: " v)]))
    stmts)))
(define (block^ . stmts) (block stmts))
(define (app^ rator . rands)
  (match rator
    [r #:when (sham:ast:rator? r) (app r rands)]
    [(sham:ast:expr:var md v) (app (rs v) rands)]
    [h #:when (hfunction? h) (apply h rands)]
    [s #:when (symbol? s) (app (rs s) rands)]
    [else (error "expected rator for app^ given: " rator)]))

(define-syntax (function^ stx)
  (syntax-parse stx
    [(_ name:expr [(args:id (~datum :) arg-types:expr) ...] ret-type:expr body:expr ...)
     #`(dfunction #f name (list (quote args) ...) (list arg-types ...) ret-type
                  (let ([args (v (quote args))] ...)
                    (block^ body ...)))]))
(define hfunction-manager
  (make-keyword-procedure
   (λ (kws kw-args . rest-args)
     (define hf (first rest-args))
     (define app-args (rest rest-args))
     (match-define (hfunction id sym-args arg-types ret-type body-builder finfo sham-module) hf)
     (match kws
       ['()
        (cond
          [(and (empty? kws) (andmap sham:ast? app-args))
           (app (rs id) app-args)]
          [(and (empty? kws) (andmap (λ (x) (or (sham:ast? x)
                                                (hfunction? x)))
                                     app-args))
           (app (rs id) (map (λ (x) (if (sham:ast? x) x
                                        (v (hfunction-id x))))
                             app-args))]
          [else (error "unknown arguments when applying function" id app-args)])]
       ['(#:inline)
        (define is-ret-void #f)
        ;;instead check the return type of the function to figure this out
        (define (set-instead-return se var)
          (define (rec se)
            (match se
              [(sham:ast:stmt:set! md l r) (sham:ast:stmt:set! l (rec r))]
              [(sham:ast:stmt:if md ch th el) (sham:ast:stmt:if (rec ch) (rec th) (rec el))]
              [(sham:ast:stmt:while md ch bdy) (sham:ast:stmt:while ch (rec bdy))]
              [(sham:ast:stmt:return md (sham:ast:expr:void md2))
               (set! is-ret-void #t)
               (svoid)]
              [(sham:ast:stmt:return md val) (sham:ast:stmt:set! var val)]
              [(sham:ast:stmt:expr md e) (sham:ast:stmt:expr (rec e))]
              [(sham:ast:stmt:block md stmts) (sham:ast:stmt:block (map rec stmts))]

              [(sham:ast:expr:app md rator rands)
               (sham:ast:expr:app rator (map rec rands))]
              [(sham:ast:expr:let md syms vals typs s e)
               (sham:ast:expr:let syms vals typs (rec s) (rec e))]
              [else se]))
          (rec se))
        (define inline-level (first kw-args))
        (when (not (equal? inline-level 1))
          (error "don't know how to inline more than once."))
        (define nsyms (map gensym sym-args))
        (define ret-sym (gensym 'ret))
        (sham:ast:expr:let (cons ret-sym nsyms) (cons ret-type arg-types) (cons (evoid) app-args)
                           (set-instead-return (apply body-builder (map v nsyms)) (v ret-sym))
                           (if is-ret-void
                               (evoid)
                               (v ret-sym)))]
       ['(#:specialize)
        (define special-args (first kw-args))
        (define special-args-val (for/list ([aa app-args]
                                            [i (in-range (length app-args))]
                                            #:when (member i special-args))
                                   aa))
        (cond
          [(is-specialized? sham-module id special-args special-args-val)
           =>
           (λ (f)
             (apply (curry app^ f)
                    (for/list ([aa app-args]
                               [i (in-range (length app-args))]
                               #:when (not (member i special-args)))
                      aa)))]
          [else (define minfo (hmodule-info sham-module))
                (define new-id (gensym id))
                (define new-body-builder
                  (λ args ;; arguments to the function after specialization
                    (define-values
                      (mix-vals final-arg-i)
                      (for/fold ([mix-vals '()] ;; values to give to old body builder
                                 [special-i 0]) ;; number of special-args seen so far
                                ([i (in-range (length sym-args))])
                        (if (member i special-args)
                            ;; if specialized argument get from app-args else from args
                            (values (cons (list-ref app-args i) mix-vals)
                                    (add1 special-i))
                            (values (cons (list-ref args (- i special-i)) mix-vals)
                                    special-i))))
                    (apply body-builder (reverse mix-vals))))
                (define new-stuff
                  (for/list ([s sym-args]
                             [t arg-types]
                             [i (in-range (length app-args))]
                             #:when (not (member i special-args)))
                    (cons s t)))
                (define new-f
                  (hfunction new-id
                             (map car new-stuff) (map cdr new-stuff) ret-type
                             new-body-builder finfo sham-module))
                (add-to-sham-module! sham-module new-f)
                (add-specialized! sham-module id new-f special-args
                                  (for/list ([aa app-args]
                                             [i (in-range (length app-args))]
                                             #:when (member i special-args))
                                    aa))
                ;; (printf "todo: memoize specialize")
                (define new-app-args
                  (for/list ([aa app-args]
                             [i (in-range (length app-args))]
                             #:when (not (member i special-args)))
                    aa))
                (apply (curry app^ new-f) new-app-args)])]))))
(define (add-specialized! sham-module id f arg-nums arg-vals)
  (define specialize-map (hash-ref! (hmodule-hinfo sham-module) '#%specialized (make-hash)))
  (hash-set! specialize-map (list id arg-nums arg-vals) f))
(define (is-specialized? sham-module id arg-nums arg-vals)
  (define specialize-map (hash-ref! (hmodule-hinfo sham-module) '#%specialized (make-hash)))
  (hash-ref specialize-map (list id arg-nums arg-vals) #f))
(define (get-specialized sham-module id arg-nums arg-vals)
  (define specialize-map (hash-ref! (hmodule-hinfo sham-module) '#%specialized (make-hash)))
  (hash-ref specialize-map (list id arg-nums arg-vals)))
(struct hfunction [id sym-args arg-types ret-type body-builder finfo sham-module]
  #:property prop:procedure hfunction-manager)
(define (sham-function-info hf)
  (match-define (hfunction id sym-args arg-types ret-type body-builder finfo sham-module) hf)
  (printf "hfunction: id: ~a, sym-args: ~a, arg-types: ~a, ret-type: ~a, finfo: ~a\n"
          id sym-args arg-types ret-type finfo)
  hf)

(struct hmodule [id func-map info hinfo (cmod #:mutable)])

(define (create-empty-sham-module (id "module") (info (make-hash)))
  (hmodule id (make-hash) info (make-hash) #f))
(define (create-sham-module funcs (id "module") (info (make-hash)))
  (define m (create-empty-sham-module id info))
  (for ([f funcs])
    (add-to-sham-module! m f))
  m)
(define (add-to-sham-module! m func)
  (cond  [(hfunction? func) (hash-set! (hmodule-func-map m) (hfunction-id func) func)]
         [(sham:def:function? func)
          (match-define (sham:def:function info id syms t ret b) func)
          (hash-set! (hmodule-func-map m) id func)]
         [(sham:def:global? func)
          (match-define (sham:def:global info id t) func)
          (hash-set! (hmodule-func-map m) id func)]))

(begin-for-syntax
  (define-splicing-syntax-class define-attrs
    (pattern (~seq (~datum #:module) mod:expr) #:attr info (cons 'module #'mod))
    (pattern (~seq (~datum #:info) i:expr) #:attr info (cons 'finfo #'i))))
(require "parameters.rkt")
(define-syntax (define-sham-function stx)
  (syntax-parse stx
    [(_ attrs:define-attrs ...
        [name:id
         (args:expr (~datum :) arg-types:expr) ... (~datum :) ret-type:expr]
        body:expr ...)
     (define mod
       (cond
         [(assoc 'module (attribute attrs.info)) => (λ (v) (cdr v))]
         [else #'(current-sham-module)]))
     #`(begin
         (define name
           (hfunction (quasiquote name)
                      (list (quasiquote args) ...)
                      (list arg-types ...)
                      ret-type
                      (λ (args ...) (block^ body ...))
                      #,(cond
                          [(assoc 'finfo (attribute attrs.info)) => (λ (v) (cdr v))]
                          [else #`(common-function-info)])
                      #,mod))
         (add-to-sham-module! #,mod name))]))
(define-syntax (define-sham-global stx)
  (syntax-parse stx
    [(_ attrs:define-attrs ... name:id type:expr)
     #`(begin
         (define name (v (quote name)))
         (add-to-sham-module! (current-sham-module) (dglobal #f (quote name) type)))]))

(define-syntax (sham-function stx)
  (syntax-parse stx
    [(_ attrs:define-attrs ...
        [name:expr
         (args:expr (~datum :) arg-types:expr) ... (~datum :) ret-type:expr]
        body:expr ...)
     #`(hfunction (quasiquote name)
                  (list (quasiquote args) ...)
                  (list arg-types ...)
                  ret-type
                  (λ (args ...) (block^ body ...))
                  #,(cond
                      [(assoc 'finfo (attribute attrs.info)) => (λ (v) (cdr v))]
                      [else #`(common-function-info)])
                  (current-sham-module))]))

(define (get-function-for-module hf)
  (match-define (hfunction id sym-args arg-types ret-type body-builder finfo sham-module) hf)
  (dfunction finfo id sym-args arg-types ret-type (apply body-builder (map v sym-args))))

(define (get-functions fs)
  (map (λ (f)
         (cond [(sham:def:function? f) f]
               [(hfunction? f) (get-function-for-module f)]
               [(sham:def:type? f) f]
               [(sham:def:global? f) f]
               [(sham:def:global-string? f) f]
               [else (error "unknown function for creating module.")]))
       fs))

(define-syntax-rule (define-module name info funcs)
  (define name (dmodule info (quote name) (get-functions funcs))))


;; (define (let1 var type val stmt expr)
;;   (let (list var) (list type) (list val) stmt expr))
;; (define (slet1 var type val stmt)
;;   (se (let (list var) (list type) (list val) stmt (evoid))))
;; (define (slet1^ var type val . stmt)
;;   (se (let (list var) (list type) (list val) (block stmt) (evoid))))
;; (define (elet1 var type val expr)
;;   (let (list var) (list type) (list val) (svoid) expr))
