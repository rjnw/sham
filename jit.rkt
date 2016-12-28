#lang racket
(require ffi/unsafe)
(require "libjit.rkt")
(require "jit-env.rkt")
(require "jit-type.rkt")
(provide (all-defined-out))

(define empty-label (jit_uint_not 0))

(struct context (jit env))
;; (define jit_type_scheme_object (jit_type_create_struct (list jit_type_short jit_type_short) 1))
;; (define jit_type_scheme_object_p (jit_type_create_pointer jit_type_scheme_object 1))

(define (create-jit-context)
  (jit_context_create))
(define global-jit-context (create-jit-context))

(define (create-initial-environment)
  (register-initial-types (empty-env)))
(define global-environment (create-initial-environment))
(define global-context (context global-jit-context global-environment))


;; returns a function object, can be given to jit_function_to_closure to get a pointer
;; (define (compile-exp exp [context global-context])
;;   (match exp
;;     [`(define (,fun-name (,args : ,types) ... : ,ret-type) ,body ...)
;;      (jit_context_build_start (context-jit context))
;;      (define sig (jit_type_create_signature 'jit_abi_cdecl (context-get-jit-type context ret-type)
;;                                             (map
;;                                              (curry context-get-jit-type context) types)
;;                                             1))
;;      (define fobj (jit_function_create (context-jit context) sig))
;;      (define env (cons
;;                   (cons fun-name (cons `(,@(map (curry context-get-jit-type context) types) -> ,ret-type) fobj))
;;                   (for/list
;;                       [(v args)
;;                        (t types)
;;                        (i (in-range (length args)))]
;;                     (cons v (cons (context-get-jit-type context t) (jit_value_get_param fobj i))))))
;;      (foldl (lambda (body env) (compile-body-exp fobj body env context)) env body)
;;      (jit_function_compile fobj)
;;      (jit_context_build_end (context-jit context))
;;      (values fobj `(,@types -> ,ret-type))]))

;; ;;returns a new environment
;; (define (compile-body-exp fobj body env context)
;;   (match body
;;     [`(define-var (,name : ,type) ,body ...)
;;      (define value (jit_value_create fobj (context-get-jit-type context type)))
;;      (define new-env (cons (cons name (cons type
;;                                             value))
;;                            env))
;;      (foldl (lambda (body env)
;;               (compile-body-exp fobj body env context)) new-env body)]
;;     [`(assign ,name ,value-exp)
;;      (jit_insn_store fobj (lookup-value env name) (compile-value-exp fobj value-exp env))
;;      env]
;;     [`(if ,value-exp-test ,body-then ,body-else)
;;      (define value-test
;;        (compile-value-exp fobj value-exp-test env))
;;      (define else-label
;;        (jit_insn_branch_if_not fobj value-test empty-label))
;;      (compile-body-exp fobj body-then env)
;;      (define end-label
;;        (jit_insn_branch fobj empty-label))
;;      (jit_insn_label fobj else-label)
;;      (define new-env
;;        (compile-body-exp fobj body-else env context))
;;      (jit_insn_label fobj end-label)
;;      new-env]
;;     [`(return ,value-exp)
;;      (jit_insn_return fobj (compile-value-exp fobj value-exp env))
;;      env]
;;     [`(return-tail ,value-exp)
;;      (jit_insn_return fobj (compile-value-exp fobj value-exp env))
;;      env]))

;no env changes to go back
;; returns jit-value and type
;; (define (compile-value-exp fobj value-exp env)
;;   (match value-exp
;;     ;; [`(,rator ,rands ...)
;;     ;;  (define-values (rator-value rator-type) (compile-value-exp fobj rator env))
;;     ;;  (for/fold ([arg-values '()] [arg-types '()]) [(arg args)]
;;     ;;    (define-values (arg-value arg-type) (compile-value-exp fobj value-exp env))
;;     ;;    (values (cons arg-value arg-values)))
;;     ;;  (map (lambda (arg) (define-values (arg-value arg-type) (compile-value-exp fobj value-exp env))
;;     ;;               (cons arg-value arg-type)) rands)
;;     ;;  (check-types )]
    

;;     [`(+ ,x1 ,x2)
;;      (jit_insn_add fobj
;;                    (compile-value-exp fobj x1 env)
;;                    (compile-value-exp fobj x2 env))]
;;     [`(- ,x1 ,x2)
;;      (jit_insn_sub fobj
;;                    (compile-value-exp fobj x1 env)
;;                    (compile-value-exp fobj x2 env))]
;;     [x #:when (symbol? x)
;;        (lookup-value env x)]
;;     [x #:when (number? x)h
;;        (jit_value_create_nint_constant fobj jit_type_uint x)]))

;; (define (context-get-jit-type context type)
;;   (define type-store (context-type-store context))
;;   (get-jit-type type-store type))
;; (define (context-get-racket-type context type)
;;   (define type-store (context-type-store context))
;;   (get-racket-type type-store type))
;; (define (lookup-value env x)
;;   (cdr (cdr (assoc x env))))

;; (define (get-function-closure fobj)
;;   (jit_function_to_closure fobj))

;; (define (compile-function f [context global-context])
;;   (define-values (fobj type) (compile-exp f))
;;   (define f-ptr (get-function-closure fobj))
;;   (cast f-ptr _pointer (_cprocedure (map (curry context-get-racket-type context) (get-input-function-types type))
;;                                     (context-get-racket-type context (get-output-function-type type)))))


(define (compile-function-definition name args types ret-type body fobject env)
  (void))

(define (compile-function-declaration function-type context)
  (define sig (type-prim-jit (env-type-prim function-type)))
  (jit_function_create (context-jit context) sig))

;; returns a binding of all the define-function as an assoc list
;; TODO support recursive struct types
(define (compile-module m [context global-context])
  (define (register-module-statement stmt env)
    (match stmt
      [`(define-type ,type-name ,t)
       (define type-decl (compile-type-declaration stmt env))
       (define type (compile-type-definition type-decl env))
       (env-extend type-name type env)]
      [`(define-function (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define type (create-type `(,@types -> ,ret-type) env))
       (define function-obj (compile-function-declaration type context))
       (env-extend function-name (env-jit-function-decl type function-obj) env)]))

  (define (compile-module-statement stmt env module-env)
    (match stmt
      [`(define-type ,type-name ,t)
       ;types are created in register phase; something for recursive struct types to be done
       (env-extend type-name (env-lookup type-name env) module-env)]

      [`(define-function (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define f (compile-function-definition function-name args types ret-type body
                                              (env-lookup function-name env) env))
       (env-extend function-name f module-env)]))

  (match m
    [`(module ,module-stmts ...)
     (define env
       (for/fold ([env (context-env context)])
                 ([stmt module-stmts])
         (register-module-statement stmt env)))
     (for/fold ([module-env (empty-env)])
               ([stmt module-stmts])
       (compile-module-statement stmt env module-env))]))

(module+ test
  (require rackunit)
  (pretty-print
   (compile-module
    '(module
         (define-type bc (struct (b : int) (c : int)))
         (define-type pbc (pointer bc))
         (define-type ui (struct (bc1 : pbc) (bc2 : pbc)))
       ))))
