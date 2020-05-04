#lang racket

(require sham/md
         sham/parameters
         sham/ir/ast/core
         sham/ir/ast/simple
         sham/ir/ast/syntax)

(require sham/private/md
         sham/private/box
         sham/private/keyword
         sham/private/maybe)

(require syntax/parse
         syntax/parse/define
         (for-syntax racket/syntax syntax/parse racket/pretty))

(provide (all-defined-out))

(struct open-value [])
(struct open-sham-env [id defs opens attrs (closed #:mutable)])

(define (add-def-to-open-sham-env! om def)
  ((add-to-list-box! (open-sham-env-defs om)) def))
(define (add-open-to-open-sham-env! om o)
  ((add-to-list-box! (open-sham-env-opens om)) o))

(define (add-to-open-sham-env! om v)
  (cond [(def? v) (add-def-to-open-sham-env! om v)]
        [(open-value? v) (add-open-to-open-sham-env! om v)]
        [else (error 'sham "unknown value for adding to sham module:~a,~a" (open-sham-env-id om) v)]))

(define (close-open-sham-env! mo)
  ;; todo should call build-sham-module
  (match-define (open-sham-env name defs ofs attrs closed) mo)
  (error 'sham:todo))

(define (build&register-open-env name vals attrs)
  (define m (open-sham-env name
                              (list-box (filter def? vals))
                              (list-box (filter open-value? vals))
                              attrs
                              #f))
  (sham-current-env m)
  m)

(define-syntax (define-current-sham-env stx)
  (syntax-parse stx
    [(_ module-name:id (~seq k:keyword v:expr) ... vals ...)
     (when (equal? (syntax-local-context) 'expression)
       (error 'sham:ir:ast "define-current-sham-module: not allowed in an expression context"))
     #`(define module-name
         (build&register-open-module `module-name
                                     (list vals ...)
                                     (list (cons `k v) ...)))]))

(define-general-keyword-procedure
  (open-sham-function-app ka of . rest-args)
  (match-define (open-sham-function name args type bodyf attrs inms compiled-app closed) of)
  (if (andmap expr? rest-args)
      (e-app name (lookup-keyword ka #:flags #f) rest-args)
      (compiled-app ka rest-args)))

(struct open-sham-function [id arg-syms type body-lambda attributes in-modules compiled-app (closed #:mutable)]
  #:property prop:procedure open-sham-function-app)

(define (close-open-sham-function! of)
  (match-define (open-sham-function name args type bodyf attrs inms compiled-app closed) of)
  (define (close)
    (define (populate-function-md! (md (empty-function-md)))
      (define attribute-map
        (list
         (cons '(#:calling-convention #:convention #:call-conv)
               set-function-md-llvm-calling-convention!)
         (cons '(#:general-attributes)
               set-function-md-llvm-general-attributes!)
         (cons '(#:return-attribute)
               set-function-md-llvm-return-attributes!)
         (cons '(#:argument-attributes)
               set-function-md-llvm-argument-attributes!)
         (cons '(#:opt-pass #:optimization-pass)
               set-function-md-llvm-optimization-pass!)
         (cons '(#:rkt-type #:racket-type)
               set-function-md-jit-rkt-type!)))
      (for ([am attribute-map])
        (match-define (cons keys setf) am)
        (cond
          [(lookup-keyword attrs keys #f)
           =>
           (curry setf md)]))
      md)
    (define md
      (cond
        [(lookup-keyword attrs #:metadata #:md #f)
         => (compose populate-function-md! hash-copy)]
        [(populate-function-md!)]))
    (define f (d-function md name type (bodyf)))
    (set-open-sham-function-closed! of f)
    f)
  (from-maybe closed close))

(define (build&register-open-function omods name arg-ids arg-types ret-type bodyf attrs)
  (printf "~a" (list omods name arg-ids arg-types ret-type attrs))
  (define in-mods
    (if (sham-current-env) (cons (sham-current-env) omods) omods))
  (define error-app
    (make-keyword-procedure
     (λ (kws kw-args of . rest-args)
       (error 'sham:function "sham function used in application before compilation ~a"
              (open-sham-function-id of)))))
  (define f (open-sham-function name arg-ids (t-function arg-types ret-type)
                                bodyf
                                attrs
                                in-mods
                                error-app
                                #f))
  (for ([m in-mods])
    (when m (add-open-to-open-sham-env! m f)))
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
                       (λ () (function-body^ (arg-ids : arg-type-names) ... body ...))
                       (list (cons `aks avs) ...))))]))

(define-simple-macro (define-sham-efunction header ... body)
  (define-sham-function header ... (return^ body)))

(define sham-function? (or/c open-sham-function? d-function?))
(define sham-define? (or/c sham-function? def?))
