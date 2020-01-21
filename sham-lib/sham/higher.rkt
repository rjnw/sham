#lang racket

(require "ast.rkt"
         "parameters.rkt")
(require (for-syntax racket/syntax syntax/parse racket/pretty))

(provide (all-defined-out))

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
(define sham-function? hfunction?)
(define (sham-function-info hf)
  (match-define (hfunction id sym-args arg-types ret-type body-builder finfo sham-module) hf)
  (printf "hfunction: id: ~a, sym-args: ~a, arg-types: ~a, ret-type: ~a, finfo: ~a\n"
          id sym-args arg-types ret-type finfo)
  hf)

(struct hmodule [id func-map info hinfo (internal #:mutable)])

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
