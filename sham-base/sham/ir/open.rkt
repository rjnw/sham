#lang racket

(require sham/md
         sham/parameters
         sham/ir/ast
         sham/ir/simple
         sham/ir/builder
         sham/llvm/ir/callconv)

(require sham/private/md
         sham/private/box
         sham/private/keyword
         sham/private/maybe)

(require syntax/parse
         syntax/parse/define
         (for-syntax racket/syntax syntax/parse))

(provide (all-defined-out))

(struct open-value [])
(struct open-sham-env open-value [id vals attrs (closed #:mutable)])
(define (open-sham-env-values e)
  (list-unbox (open-sham-env-vals e)))
(define (add-to-open-sham-env! om v)
  (if (or (def? v) (open-sham-function? v))
      ((add-to-list-box! (open-sham-env-vals om)) v)
      (error 'sham "unknown value for adding to sham module:~a,~a" (open-sham-env-id om) v)))

(define (close-open-sham-env! om)
  (match-define (open-sham-env name vals attrs closed) om)
  (define (close-value v)
    (if (open-sham-function? v)
        (close-open-sham-function! v)
        v))
  (define (close)
    (define md
      (keyword-apply sham-module-metadata
                     (map car attrs)
                     (map cdr attrs)
                     (flatten (lookup-keyword attrs #:metadata #:md '()))))
    (define mast (d-module md name (map close-value (list-unbox vals))))
    (set-open-sham-env-closed! om mast)
    mast)
  (from-maybe closed close))

(define (build&register-open-env name vals attrs)
  (define m (open-sham-env name (list-box vals) attrs #f))
  (sham-current-env m)
  m)

(define-syntax (define-current-sham-env stx)
  (syntax-parse stx
    [(_ module-name:id (~seq k:keyword v:expr) ... vals ...)
     (when (equal? (syntax-local-context) 'expression)
       (error 'sham:ir:ast "define-current-sham-module: not allowed in an expression context"))
     #`(define module-name
         (build&register-open-env `module-name
                                  (list vals ...)
                                  (list (cons `k v) ...)))]))

(define-general-keyword-procedure
  (open-sham-function-app ka of . rest-args)
  (match-define (open-sham-function name args type bodyf attrs inms compiled-app closed) of)
  (define (combine-with-def-attrs flgs attrs)
    (define flags (from-maybe flgs empty-instruction-md))
    (cond
      [(lookup-keyword attrs #:calling-convention #:convention #:call-conv (const #f))
       => (curry callconv! flags)]
      [(lookup-keyword attrs #:md #:metatdata (const #f))
       => (λ (md)
            (when-function-md-llvm-calling-convention
             md conv (callconv! flags conv)))])
    flags)
  (if (andmap expr? rest-args)
      (e-app name (combine-with-def-attrs (lookup-keyword ka #:flags #f) attrs) rest-args)
      (apply compiled-app rest-args)))

(struct open-sham-function [id arg-syms type body-lambda attributes in-modules (compiled-app #:mutable) (closed #:mutable)]
  #:property prop:procedure open-sham-function-app)

(define (close-open-sham-function! of)
  (match-define (open-sham-function name args type bodyf attrs inms compiled-app closed) of)
  (define (close)
    (define md
      (keyword-apply sham-function-metadata
                     (map car attrs)
                     (map cdr attrs)
                     (flatten (lookup-keyword attrs #:metadata #:md '()))))
    (define f (d-function md name type (bodyf)))
    (set-open-sham-function-closed! of f)
    f)
  (from-maybe closed close))

(define (build&register-open-function omods name arg-ids arg-types ret-type bodyf attrs)
  (define in-mods
    (if (sham-current-env) (cons (sham-current-env) omods) omods))
  (define error-app
    (make-keyword-procedure
     (λ (kws kw-args . rest-args)
       (error 'sham:function "sham function used in application before compilation ~a" name))))
  (define f (open-sham-function name arg-ids (t-function arg-types ret-type)
                                bodyf
                                attrs
                                in-mods
                                error-app
                                #f))
  (for ([m in-mods])
    (when m (add-to-open-sham-env! m f)))
  f)

(define-syntax (define-sham-function stx)
  (syntax-parse stx
    [(_ (~alt (~seq (~datum #:module) mods)
              (~seq aks:keyword avs:expr)) ...
        (name:id (arg-ids:id (~optional (~datum :)) arg-types) ... (~datum :) ret-type)
        body ...)
     #:with (arg-type-names ...) (generate-temporaries #`(arg-ids ...))
     (when (equal? (syntax-local-context) 'expression)
       (error 'sham:ir:ast "define-sham-function: not allowed in an expression context"))
     #`(define name (let ([arg-type-names arg-types] ...)
                      (build&register-open-function
                       (list mods ...)
                       `name `(arg-ids ...) (list arg-type-names ...) ret-type
                       (λ () (function-body^ [(arg-ids : arg-type-names) ...] body ...))
                       (list (cons `aks avs) ...))))]))

(define-simple-macro (define-sham-efunction header ... body)
  (define-sham-function header ... (return^ body)))

(define sham-function? (or/c open-sham-function? d-function?))
(define sham-define? (or/c sham-function? def?))
