#lang racket
(require ffi/unsafe)
(require "libjit.rkt")
(require "jit-env.rkt")
(require "jit-type.rkt")
(require "jit-intr.rkt")
(provide compile-module
         create-jit-context
         create-initial-environment
         context)

(define empty-label (jit_uint_not 0))

(struct context (jit env))

(define (create-jit-context)
  (jit_context_create))
(define global-jit-context (create-jit-context))

(define (create-initial-environment)
  (register-jit-internals (register-initial-types (empty-env))))
(define global-environment (create-initial-environment))
(define global-context (context global-jit-context global-environment))

(define (get-jit-function-pointer fobj)
  (jit_function_to_closure fobj))

(define (jit-get-function f)
  (match f
    [(env-jit-function type object cpointer)
     (racket-type-cast cpointer type-void* type)]))

(define (create-value value type function env)
  (define envtype (env-lookup type env))
  (define prim-type (type-prim-jit (env-type-prim envtype)))
  (cond [(or (type-native-int? envtype)
             (type-pointer? envtype))
         (jit_value_create_nint_constant
          function
          prim-type
          value)]
        [(type-float32? type)
         (jit_value_create_float32_constant
          function
          prim-type
          value)]
        [else (error "value type not supported yet!" value type)]))

(define (compile-lhs-assign lhs exp-value function env)
  (match exp
    [(? symbol?)
     (define lhs-v (env-jit-value-v (env-lookup lhs env)))
     (jit_insn_store function lhs-v exp-value)]
    [else (error "not implemented" exp)]))

(define (compile-lhs-expression exp function env)
  (match exp
    [(? symbol?) (env-jit-value-v (env-lookup exp env))]
    [else (error "not implemented lhs expression" exp)]))

(define (compile-app rator rand-values function env)
  (match (env-lookup rator env)
    [(env-jit-function type object cpointer)
     (jit_insn_call function "" object (type-prim-jit (env-type-prim type)) rand-values 0)]
    [(env-jit-function-decl type object)
     (jit_insn_call function "" object (type-prim-jit (env-type-prim type)) rand-values 0)]
    [(env-jit-internal-function compiler)
     (compiler function rand-values)]
    [(env-racket-function type f)
     ;TODO
     (error "not implemented applicative")]
    [(env-racket-ffi-function type f)
     ;TODO
     (error "not implemented applicative")]
    [(env-c-function type f)
     (jit_insn_call_native function #f f type rand-values 0)]
    [else (printf "rator ~a\n" (env-lookup rator env))]))

;; returns an object of jit_value
(define (compile-expression exp function env)
  (match exp
    [`(#%app ,rator ,rands ...)
     (define rand-values
       (for/list ([rand rands])
         (compile-expression rand function env)))
     (compile-app rator rand-values function env)]
    ;; [`(#%app (racket ,rator) ,rands ...)]
    ;; [`(#%app (jit ,rator) ,rands ...)]
    ;; [`(#%app (prim ,rator) ,rands ...)]
    [`(#%value ,value ,type) (create-value value type function env)]
    [else (compile-lhs-expression exp function env)]))

;; returns void
(define (compile-statement stmt function env)
  (match stmt
    [`(define-variable (,id : ,type) ,st)
     (define id-type (env-lookup type env))
     (define id-value (jit_value_create function (type-prim-jit (env-type-prim id-type))))
     (compile-statement function (env-extend id (env-jit-value id-value id-type) env))]
    [`(assign ,lhs ,exp)
     (define exp-value (compile-expression exp function env))
     (compile-lhs-assign lhs exp-value function env)]
    [`(if ,tst ,thn ,els)
     (define tst-value (compile-expression tst function env))
     (define label-if (jit_insn_branch_if function tst-value empty-label))
     (compile-statement els function env)
     (define label-end (jit_insn_branch function empty-label))
     (jit_insn_label function label-if)
     (compile-statement thn function env)
     (jit_insn_label function label-end)]
    [`(while ,tst ,body)
     (define start-label (jit_insn_label function empty-label))
     (define tst-value (compile-expression tst function env))
     (define end-label (jit_insn_branch_if_not function tst-value empty-label))
     (compile-statement body function env)
     (jit_insn_branch function start-label)
     (jit_insn_label end-label)]
    [`(return ,exp) (jit_insn_return function (compile-expression exp function env))]
    [`(return-tail ,exp) (jit_insn_return function (compile-expression exp function env))];;TODO
    [`(block ,stmts ...)
     (for ([stmt stmts])
       (compile-statement stmt function env))]
    [else (error "unknown statement or not implemented")]))

(define (compile-function-definition name args types ret-type body f-decl env context)
  (define fobject (env-jit-function-decl-object f-decl))
  (define ftype (env-jit-function-decl-type f-decl))
  (define jitc (context-jit context))
  (jit_context_build_start jitc)
  (define new-env
    (for/fold ([env env])
             ([arg args]
              [type types]
              [i (in-range (length args))])
      (env-extend arg (env-jit-value (jit_value_get_param fobject i) type) env)))
  (compile-statement body fobject new-env)
  (jit_function_compile fobject)
  (jit_context_build_end jitc)
  (env-jit-function ftype fobject (get-jit-function-pointer fobject)))

(define (compile-function-declaration function-type context)
  (define sig (type-prim-jit (env-type-prim function-type)))
  (jit_context_build_start (context-jit context))
  (define function-obj (jit_function_create (context-jit context) sig))
  (jit_context_build_end (context-jit context))
  function-obj)

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
                                              (env-lookup function-name env) env context))
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
  (define module-env
   (compile-module
    '(module
         (define-type bc (struct (b : int) (c : int)))
         (define-type pbc (pointer bc))
       (define-type ui (struct (bc1 : pbc) (bc2 : pbc)))
       (define-function (f (x : int) : int)
         (return (#%app jit-add x x)))
       (define-function (even? (x : int) : int)
         (if (#%app jit-eq? (#%app jit-rem x (#%value 2 int)) (#%value 0 int))
             (return (#%value 1 int))
             (return (#%value 0 int))))
       )))
  (define f (jit-get-function (env-lookup 'f module-env)))
  (pretty-print (f 21))
  (define even? (jit-get-function (env-lookup 'even? module-env)))
  (pretty-print (even? 42)))
