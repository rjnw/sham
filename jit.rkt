#lang racket
(require ffi/unsafe)

(require "libjit.rkt")

(require "jit-env.rkt")
(require "jit-type.rkt")
(require "jit-intr.rkt")
(require "jit-expand.rkt")

(provide compile-module
         create-jit-context
         create-initial-environment
         jit-get-function
         jit-dump-function
         jit-get-racket-type
         env-lookup
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

(define _fopen (get-ffi-obj "fopen" #f (_fun _string _string -> _pointer)))
(define _fclose (get-ffi-obj "fclose" #f (_fun _pointer -> _int)))
(define (jit-dump-function fobject (filename "/tmp/jitdump") (function-name ""))
  (define fptr (_fopen filename "a"))
  (jit_dump_function fptr (env-jit-function-object fobject) function-name)
  (_fclose fptr))

(define (jit-get-function f)
  (match f
    [(env-jit-function type object cpointer)
     (racket-type-cast cpointer type-void* type)]))

(define (jit-get-racket-type t)
  (type-prim-racket (env-type-prim t)))

(define (create-value value type function env)
  (define envtype (env-lookup type env))
  (define prim-type (type-prim-jit (env-type-prim envtype)))
  (cond [(or (type-native-int? envtype)
             (type-pointer? (env-type-skel envtype)))
         (jit_value_create_nint_constant
          function
          prim-type
          value)]
        [(type-float32? envtype)
         (jit_value_create_float32_constant
          function
          prim-type
          value)]
        [else (error "value type not supported yet!" value type)]))

(define (compile-lhs-set lhs exp-value function env)
  (match lhs
    [(? symbol?)
     (define lhs-v (env-jit-value-v (env-lookup lhs env)))
     (jit_insn_store function lhs-v exp-value)]
    [`(* ,ptr ,off : ,type) ;off should be number of bytes, we don't do pointer arithmatic
     (define ptr-value (compile-expression ptr function env))
     (define off-value (compile-expression off function env))
     (define ptr-with-off (jit_insn_add function ptr-value off-value))
     (jit_insn_store_relative function ptr-with-off 0 exp-value)]
    [else (error "not implemented" lhs)]))

(define (compile-lhs-expression exp function env)
  (match exp
    [(? symbol?) (env-jit-value-v (env-lookup exp env))]
    [`(* ,ptr ,off : ,type) ;; again off is in bytes
     (define value-type (type-prim-jit
                       (env-type-prim
                        (env-lookup type env))))
     (define ptr-value (compile-expression ptr function env))
     (define off-value (compile-expression off function env))
     (define ptr-with-off (jit_insn_add function ptr-value off-value))
     (jit_insn_load_relative function ptr-with-off 0 value-type)]
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
    [else (error "rator ~a\n" (env-lookup rator env))]))

;; returns an object of jit_value
(define (compile-expression exp function env)
  ;; (printf "compiling expression ~a\n" exp)
  (match exp
    [`(#%app ,rator ,rands ...)
     (define rand-values
       (for/list ([rand rands])
         (compile-expression rand function env)))
     (compile-app rator rand-values function env)]
    [`(#%value ,value ,type) (create-value value type function env)]
    [`(#%offset ,type ,field)
     (compile-expression `(#%value ,(get-struct-offset type field env) int) function env)]
    [`(#%sizeof ,type)
     (define envtype (env-lookup type env))
     (create-value (jit_type_get_size (type-prim-jit (env-type-prim envtype)))
                   'uint function env)] 
    [else (compile-lhs-expression exp function env)]))

;; returns void
(define (compile-statement stmt function env)
  ;; (printf "compiling statement ~a\n" stmt)
  (match (statement-expander stmt)
    [`(let ((,ids : ,types) ...) ,st)
     (define id-types (map (curryr env-lookup env) types))
     (define id-values
       (map (λ (id-type)
	      (jit_value_create function
				(type-prim-jit (env-type-prim id-type))))
	    id-types))
     (define new-env
       (for/fold ([env env])
	   ([id-type id-types]
	    [id ids]
	    [id-value id-values])
	 (env-extend id (env-jit-value id-value id-type) env)))
     (compile-statement st function new-env)]
    [`(set! ,lhs ,v)
     (define exp-value (compile-expression v function env))
     (compile-lhs-set lhs exp-value function env)]
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
     (jit_insn_label function end-label)]
    [`(return ,exp) (jit_insn_return function
				     (compile-expression exp function env))]
    [`(return-tail ,exp) (jit_insn_return function
					  (compile-expression exp function env))] ;;TODO
    [`(block ,stmts ...)
     (for ([stmt stmts])
       (compile-statement stmt function env))]
    [`(#%exp ,e)
     (compile-expression e function env)]
    [else (error "unknown statement or not implemented" stmt)]))

(define (compile-function-definition name args types ret-type
				     body f-decl env context)
  (define fobject (env-jit-function-decl-object f-decl))
  (define ftype (env-jit-function-decl-type f-decl))
  (define jitc (context-jit context))
  (jit_context_build_start jitc)
  (define new-env
    (for/fold ([env env])
             ([arg args]
              [type types]
              [i (in-range (length args))])
      (env-extend arg
		  (env-jit-value (jit_value_get_param fobject i) type)
		  env)))
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
       (env-extend function-name
		   (env-jit-function-decl type function-obj)
		   env)]))

  (define (compile-module-statement stmt env module-env)
    (match stmt
      [`(define-type ,type-name ,t)
       ;types are created in register phase; something for recursive struct types to be done
       (env-extend type-name (env-lookup type-name env) module-env)]

      [`(define-function (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define f (compile-function-definition function-name args types ret-type
					      body (env-lookup function-name env)
					      env context))
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
  (require racket/unsafe/ops)
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

       (define-function (meven? (x : int) : int)
         (if (#%app jit-eq? x (#%value 0 int))
             (return (#%value 1 int))
             (return (#%app modd? (#%app jit-sub x (#%value 1 int))))))

       (define-function (modd? (x : int) : int)
         (if (#%app jit-eq? x (#%value 0 int))
             (return (#%value 0 int))
             (return (#%app meven?
                            (#%app jit-sub x (#%value 1 int))))))

       (define-function (fact (x : int) : int)
         (if (#%app jit-eq? x (#%value 0 int))
             (return (#%value 1 int))
             (return (#%app jit-mul
                     x
                     (#%app fact
                            (#%app jit-sub x (#%value 1 int)))))))

       (define-function (factr (x : int) : int)
         (let ((result : int))
           (block
            (set! result (#%value 1 int))
            (let ((i : int))
              (block
               (set! i (#%value 1 int))
               (while (#%app jit-le? i x)
                 (block
                  (set! result (#%app jit-mul result i))
                  (set! i (#%app jit-add i (#%value 1 int)))))
               (return result))))))

       (define-function (malloc-test  : int)
         (let ((ptr : void*))
           (let ((x : int))
             (block
              (set! ptr (#%app jit-malloc (#%sizeof int)))
              (set! (* ptr (#%value 0 int) : int) (#%value 8 int))
              (set! x (* ptr (#%value 0 int) : int))
              (#%exp (#%app jit-free ptr))
              (return x)))))
       (define-type int* (pointer int))

       (define-function (sum-array (arr : int*) (size : int) : int)
         (let ((i : int))
           (let ((sum : int))
             (block
              (set! i (#%value 0 int))
              (set! sum (#%value 0 int))
              (while (#%app jit-lt? i size)
                (block
                 (set! sum (#%app jit-add
                                    sum
                                    (* arr (#%app jit-mul i (#%sizeof int)) : int)))
                 (set! i (#%app jit-add i (#%value 1 int)))))
              (return sum)))))

       (define-type ulong* (pointer ulong))
       (define-function (dot-product (arr1 : ulong*) (arr2 : ulong*)
				     (size : ulong) : ulong)
         (let ((sum : ulong)
                            (i : ulong))
           (block
            (set! i (#%value 0 ulong))
            (set! sum (#%value 0 ulong))
            (while (#%app jit-lt? i size)
              (let ((ptr-pos : int))
                (block
                 (set! ptr-pos (#%app jit-mul i (#%sizeof ulong)))
                 (set! sum (#%app jit-add
                                    sum
                                    (#%app jit-mul
                                           (* arr1 ptr-pos : ulong)
                                           (* arr2 ptr-pos : ulong))))
                 (set! i (#%app jit-add i (#%value 1 ulong))))))
            (return sum))))
       (define-type float32-p (pointer float32))
       (define-type array-real (struct (size : int) (data : float32-p)))
       (define-type array-real-p (pointer array-real))
       (define-function (get-size (arr : array-real-p) : int)
         (return (* arr (#%offset array-real size) : int)))
       
       (define-function (test : int)
         (let ((ap : array-real-p))
           (block (set! ap (#%app jit-malloc (#%sizeof array-real)))
                  (set! (* ap (#%offset array-real size) : int) (#%value 42 int))
                  (return (#%app get-size ap)))))
       )))

  (define f (jit-get-function (env-lookup 'f module-env)))
  ;; (dump-jit-function (env-jit-function-object (env-lookup 'dot-product module-env))
  ;;                    "dot-product" "/tmp/jitdump")
  (pretty-print (f 21))
  (define even? (jit-get-function (env-lookup 'even? module-env)))
  (pretty-print (even? 42))
  (define meven? (jit-get-function (env-lookup 'meven? module-env)))
  (pretty-print (meven? 21))

  (define fact (jit-get-function (env-lookup 'fact module-env)))
  (pretty-print (fact 5))
  (define factr (jit-get-function (env-lookup 'factr module-env)))
  (pretty-print (factr 5))
  (define malloc-test (jit-get-function (env-lookup 'malloc-test module-env)))
  (malloc-test)
  (define sum-array (jit-get-function (env-lookup 'sum-array module-env)))
  (define biglist (stream->list (in-range 1000000)))
  
  (define bigvec (for/vector ([i (in-range 1000000)])
                   i))
  ;; (sum-array bigarray (length biglist))

  (define dot-product (jit-get-function (env-lookup 'dot-product module-env)))
  (define bigarray (list->cblock biglist _ulong))
  (time (begin
          (dot-product bigarray bigarray (length biglist))
          (dot-product bigarray bigarray (length biglist))
          (dot-product bigarray bigarray (length biglist))
          (dot-product bigarray bigarray (length biglist))))
  (time
   (begin(for/sum [(a1 (in-vector bigvec))
              (a2 (in-vector bigvec))]
           (unsafe-fx* a1 a2))
         (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
           (unsafe-fx* a1 a2))
         (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
           (unsafe-fx* a1 a2))
         (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
           (unsafe-fx* a1 a2))))
  (define test (jit-get-function (env-lookup 'test module-env)))
  (pretty-print (test))

  )


;;struct tests


