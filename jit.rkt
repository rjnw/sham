#lang racket
(require ffi/unsafe)

(require "llvm/ffi/all.rkt")

(require "jit-env.rkt")
(require "jit-type.rkt")
(require "jit-intr.rkt")
(require "jit-expand.rkt")

;; (provide compile-module
;;          create-jit-context
;;          create-initial-environment
;;          jit-get-function
;;          jit-dump-function
;;          jit-dump-module
;;          jit-get-racket-type
;;          env-lookup
;;          context)

(define (create-jit-context)
  (LLVMContextCreate))
(define global-jit-context (LLVMGetGlobalContext))

;; (define (create-initial-environment context)
;;   (register-jit-internals
;;    (register-initial-types (empty-env) context)
;;    context))
;; (define global-environment (create-initial-environment global-jit-context))
;; (define global-context (context global-jit-context global-environment))

;; (define (get-jit-function-pointer fobj)
;;   (jit_function_to_closure fobj))

;; (define _fopen (get-ffi-obj "fopen" #f (_fun _string _string -> _pointer)))
;; (define _fclose (get-ffi-obj "fclose" #f (_fun _pointer -> _int)))
;; (define (jit-dump-module mod (filename "/tmp/jitdump"))
;;   (define fptr (_fopen filename "a"))
;;   (for ([p mod])
;;     (cond
;;       [(env-jit-function? (cdr p))
;;        (jit_dump_function fptr (env-jit-function-object (cdr p)) (symbol->string (car p)))]
;;       [(env-type? (cdr p))
;;        (jit_dump_type fptr (type-prim-jit (env-type-prim (cdr p))))]))

;;   (_fclose fptr))
;; (define (jit-dump-function fobject (filename "/tmp/jitdump") (function-name ""))
;;   (define fptr (_fopen filename "a"))
;;   (jit_dump_function fptr (env-jit-function-object fobject) function-name)
;;   (_fclose fptr))

;; (define (jit-get-function f)
;;   (match f
;;     [(env-jit-function type object cpointer)
;;      (racket-type-cast cpointer type-void* type)]))

;; (define (jit-get-racket-type t)
;;   (type-prim-racket (env-type-prim t)))

;; (define (create-value value type function env)
;;   (define envtype (env-lookup type env))
;;   (define prim-type (type-prim-jit (env-type-prim envtype)))
;;   (cond [(or (type-native-int? envtype)
;;              (type-pointer? (env-type-skel envtype)))
;;          (jit_value_create_nint_constant
;;           function
;;           prim-type
;;           value)]
;;         [(type-float32? envtype)
;;          (jit_value_create_float32_constant
;;           function
;;           prim-type
;;           value)]
;;         [else (error "value type not supported yet!" value type)]))

;; (define (compile-lhs-set lhs exp-value function env)
;;   (match lhs
;;     [(? symbol?)
;;      (define lhs-v (env-jit-value-v (env-lookup lhs env)))
;;      (jit_insn_store function lhs-v exp-value)]
;;     [`(* ,ptr ,off : ,type) ;off should be number of bytes, we don't do pointer arithmatic
;;      (define ptr-value (compile-expression ptr function env))
;;      (define off-value (compile-expression off function env))
;;      (define ptr-with-off (jit_insn_add function ptr-value off-value))
;;      (jit_insn_store_relative function ptr-with-off 0 exp-value)]
;;     [else (error "not implemented" lhs)]))

;; (define (compile-lhs-expression exp function env)
;;   (match exp
;;     [(? symbol?) (env-jit-value-v (env-lookup exp env))]
;;     [`(* ,ptr ,off : ,type) ;; again off is in bytes
;;      (define value-type (type-prim-jit
;;                        (env-type-prim
;;                         (env-lookup type env))))
;;      (define ptr-value (compile-expression ptr function env))
;;      (define off-value (compile-expression off function env))
;;      (define ptr-with-off (jit_insn_add function ptr-value off-value))
;;      (jit_insn_load_relative function ptr-with-off 0 value-type)]
;;     [else (error "not implemented lhs expression" exp)]))

;; (define (compile-app rator rand-values function env)
;;   (match (env-lookup rator env)
;;     [(env-jit-function type object cpointer)
;;      (jit_insn_call function (symbol->string rator) object
;;                     (type-prim-jit (env-type-prim type)) rand-values 0)]
;;     [(env-jit-function-decl type object)
;;      (jit_insn_call function (symbol->string rator) object
;;                     (type-prim-jit (env-type-prim type)) rand-values 0)]
;;     [(env-jit-internal-function compiler)
;;      (compiler function rand-values)]
;;     [(env-racket-function type f)
;;      ;TODO
;;      (error "not implemented applicative")]
;;     [(env-racket-ffi-function type f)
;;      ;TODO
;;      (error "not implemented applicative")]
;;     [(env-c-function type f)
;;      (jit_insn_call_native function #f f type rand-values 0)]
;;     [else (error "rator ~a\n" (env-lookup rator env))]))

;; ;; returns an object of jit_value
;; (define (compile-expression exp function env)
;;   ;; (printf "compiling expression ~a\n" exp)
;;   (match exp
;;     [`(#%app ,rator ,rands ...)
;;      (define rand-values
;;        (for/list ([rand rands])
 ;;          (compile-expression rand function env)))
;;      (compile-app rator rand-values function env)]
;;     [`(#%value ,value ,type) (create-value value type function env)]
;;     [`(#%offset ,type ,field)
;;      (compile-expression `(#%value ,(get-struct-offset type field env) int) function env)]
;;     [`(#%sizeof ,type)
;;      (define envtype (env-lookup type env))
;;      (create-value (jit_type_get_size (type-prim-jit (env-type-prim envtype)))
;;                    'uint function env)]
;;     [else (compile-lhs-expression exp function env)]))

;; returns new env  
(define (statement-builder stmt function-ref env jit-module jit-builder)
  (define (build-statement stmt env)
    (match (statement-expander stmt)
      [`(let ((,ids : ,types ,vals) ...) ,st)
       (define id-types (map (curryr env-lookup env) types))
       (define new-env
         (for/fold ([env env])
                   ([id-type id-types]
                    [id ids]
                    [val vals])
           (define val-ref (build-expression val env))
           (env-extend id (env-jit-value val-ref id-type) env)))
       (build-statement st new-env)]

      [`(set! ,lhs ,v)
       (define v-ref (compile-expression v function env))
       (define lhs-v (env-lookup lhs env))
       ;;TODO verify the order of load store
       (env-extend lhs (env-jit-value v-ref (env-jit-value-type lhs-v)))]

      [`(if ,tst ,thn ,els)
       (define tst-value (build-expression tst env))
       (define then-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "then"))
       (define else-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "else"))
       (define end-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "ifend"))
       (LLVMBuildCondBr jit-builder tst-value then-block else-block)

       (LLVMPositionBuilderAtEnd jit-builder then-block)
       (build-statement els env)
       (LLVMBuildBr jit-builder end-block)

       (LLVMPositionBuilderAtEnd jit-builder else-block)
       (build-statement thn env)
       (LLVMBuildBr jit-builder end-block)

       ;;TODO add phi nodes and return correct env
       (LLVMPositionBuilderAtEnd jit-builder end-block)
       env]

      [`(while ,tst ,body)
       (define tst-value (build-expression tst env))
       (define loop-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "loop-block"))
       (define afterloop-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "afterloop-block"))

       (LLVMBuildCondBr jit-builder tst-value loop-block afterloop-block)

       (LLVMPositionBuilderAtEnd jit-builder loop-block)
       (build-statement body env)
       (LLVMBuildBr jit-builder afterloop-block)

       ;;TODO add phi nodes
       (LLVMPositionBuilderAtEnd jit-builder afterloop-block)]
      [`(return ,v)
       (LLVMBuildRet jit-builder (build-expression v env))]
      [`(block ,stmts ...)
       (for ([stmt stmts])
         ;;TODO merge environments
         (build-statement stmt env))]
      [`(#%exp ,e)
       (build-expression e env)]
      [else (error "unknown statement or not implemented" stmt)]))
  (define (build-expression e env)
    )
  (build-statement stmt env))

(define (compile-function-definition name args types ret-type
				     body env-function env jit-module jit-builder)
  (define fref (env-jit-function-ref env-function))
  (define ftype (env-jit-function-type env-function))
  (define new-env
    (for/fold ([env env])
             ([arg args]
              [type types]
              [i (in-range (length args))])
      (env-extend arg
		  (env-jit-value (LLVMGetParam fref i) type)
		  env)))
  (define entry-block (LLVMAppendBasicBlockInContext
                       (LLVMGetModuleContext jit-module) fref "entry"))
  (build-statement body fref new-env entry-block jit-module jit-builder)
  env-function)

(define (compile-function-declaration env-function-type function-name jit-module)
  (define function-type (type-prim-jit (env-type-prim env-function-type)))
  (LLVMAddFunction jit-module function-name function-type))


(define (compile-module m [module-name "module"] [context global-jit-context])
  (define jit-module (LLVMModuleCreateWithNameInContext module-name context))
  (define jit-builder (LLVMCreateBuilderInContext context))

  (define (register-module-statement stmt env)
    (match stmt
      [`(define-type ,type-name ,t)
       (define type-decl (compile-type-declaration stmt env))
       (define type (compile-type-definition type-decl env))
       (env-extend type-name type env)]
      [`(define-function (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define type (create-type `(,@types -> ,ret-type) env))
       (define function-obj (compile-function-declaration type jit-module))
       (env-extend function-name
        	   (env-jit-function function-obj type)
        	   env)]))

  (define (compile-module-statement stmt env module-env) ;; TODO return values of name and obj
    (match stmt
      [`(define-type ,type-name ,t)
       ;types are created in register phase; something for recursive struct types to be done
       ;; TODO for struct switch to creating empty struct type and then adding fields here
       (env-extend type-name (env-lookup type-name env) module-env)]
      [`(define-function (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define f (compile-function-definition function-name args types ret-type
        				      body (env-lookup function-name env)
        				      env jit-module jit-builder))
       (env-extend function-name f module-env)]))
  (void)
  ;; (match m
  ;;   [`(module ,module-stmts ...)
  ;;    (define env
  ;;      (for/fold ([env (context-env context)])
  ;;                ([stmt module-stmts])
  ;;        (register-module-statement stmt env)))
  ;;    (for/fold ([module-env (empty-env)])
  ;;              ([stmt module-stmts])
  ;;      (compile-module-statement stmt env module-env))])
  )


;; (module+ test
;;   (require racket/unsafe/ops)
;;   (require rackunit)
;;   (define module-env
;;     (compile-module
;;     '(module
;;          (define-type bc (struct (b : int) (c : int)))
;;          (define-type pbc (pointer bc))
;;        (define-type ui (struct (bc1 : pbc) (bc2 : pbc)))

;;        (define-function (f (x : int) : int)
;;          (return (#%app jit-add x x)))

;;        (define-function (even? (x : int) : int)
;;          (if (#%app jit-eq? (#%app jit-rem x (#%value 2 int)) (#%value 0 int))
;;              (return (#%value 1 int))
;;              (return (#%value 0 int))))

;;        (define-function (meven? (x : int) : int)
;;          (if (#%app jit-eq? x (#%value 0 int))
;;              (return (#%value 1 int))
;;              (return (#%app modd? (#%app jit-sub x (#%value 1 int))))))

;;        (define-function (modd? (x : int) : int)
;;          (if (#%app jit-eq? x (#%value 0 int))
;;              (return (#%value 0 int))
;;              (return (#%app meven?
;;                             (#%app jit-sub x (#%value 1 int))))))

;;        (define-function (fact (x : int) : int)
;;          (if (#%app jit-eq? x (#%value 0 int))
;;              (return (#%value 1 int))
;;              (return (#%app jit-mul
;;                      x
;;                      (#%app fact
;;                             (#%app jit-sub x (#%value 1 int)))))))

;;        (define-function (factr (x : int) : int)
;;          (let ((result : int))
;;            (block
;;             (set! result (#%value 1 int))
;;             (let ((i : int))
;;               (block
;;                (set! i (#%value 1 int))
;;                (while (#%app jit-le? i x)
;;                  (block
;;                   (set! result (#%app jit-mul result i))
;;                   (set! i (#%app jit-add i (#%value 1 int)))))
;;                (return result))))))

;;        (define-function (malloc-test  : int)
;;          (let ((ptr : void*))
;;            (let ((x : int))
;;              (block
;;               (set! ptr (#%app jit-malloc (#%sizeof int)))
;;               (set! (* ptr (#%value 0 int) : int) (#%value 8 int))
;;               (set! x (* ptr (#%value 0 int) : int))
;;               (#%exp (#%app jit-free ptr))
;;               (return x)))))
;;        (define-type int* (pointer int))

;;        (define-function (sum-array (arr : int*) (size : int) : int)
;;          (let ((i : int))
;;            (let ((sum : int))
;;              (block
;;               (set! i (#%value 0 int))
;;               (set! sum (#%value 0 int))
;;               (while (#%app jit-lt? i size)
;;                 (block
;;                  (set! sum (#%app jit-add
;;                                     sum
;;                                     (* arr (#%app jit-mul i (#%sizeof int)) : int)))
;;                  (set! i (#%app jit-add i (#%value 1 int)))))
;;               (return sum)))))

;;        (define-type ulong* (pointer ulong))
;;        (define-type uint* (pointer uint))

;;        (define-function (dot-product (arr1 : uint*) (arr2 : uint*)
;; 				     (size : uint) : uint)
;;          (let ((sum : uint)
;;                             (i : uint))
;;            (block
;;             (set! i (#%value 0 uint))
;;             (set! sum (#%value 0 uint))
;;             (while (#%app jit-lt? i size)
;;               (let ((ptr-pos : int))
;;                 (block
;;                  (set! ptr-pos (#%app jit-mul i (#%sizeof uint)))
;;                  (set! sum (#%app jit-add
;;                                     sum
;;                                     (#%app jit-mul
;;                                            (* arr1 ptr-pos : uint)
;;                                            (* arr2 ptr-pos : uint))))
;;                  (set! i (#%app jit-add i (#%value 1 uint))))))
;;             (return sum))))
;;        (define-type float32-p (pointer float32))
;;        (define-type array-real (struct (size : int) (data : float32-p)))
;;        (define-type array-real-p (pointer array-real))
;;        (define-function (get-size (arr : array-real-p) : int)
;;          (return (* arr (#%offset array-real size) : int)))
       
;;        (define-function (test : int)
;;          (let ((ap : array-real-p))
;;            (block (set! ap (#%app jit-malloc (#%sizeof array-real)))
;;                   (set! (* ap (#%offset array-real size) : int) (#%value 42 int))
;;                   (return (#%app get-size ap)))))
;;        )))

;;   (define f (jit-get-function (env-lookup 'f module-env)))
;;   (jit-dump-module module-env)
;;   ;; (jit-dump-function (env-lookup 'fact module-env)
;;   ;;                     "/tmp/jitdump" "fact")
;;   ;; (pretty-print (f 21))
;;   ;; (define even? (jit-get-function (env-lookup 'even? module-env)))
;;   ;; (pretty-print (even? 42))
;;   ;; (define meven? (jit-get-function (env-lookup 'meven? module-env)))
;;   ;; (pretty-print (meven? 21))

;;   ;; (define fact (jit-get-function (env-lookup 'fact module-env)))
;;   ;; (pretty-print (fact 5))
;;   ;; (define factr (jit-get-function (env-lookup 'factr module-env)))
;;   ;; (pretty-print (factr 5))
;;   ;; (define malloc-test (jit-get-function (env-lookup 'malloc-test module-env)))
;;   ;; (malloc-test)
;;   ;; (define sum-array (jit-get-function (env-lookup 'sum-array module-env)))
;;   ;; (define biglist (stream->list (in-range 1000000)))
  
;;   ;; (define bigvec (for/vector ([i (in-range 1000000)])
;;   ;;                  i))
;;   ;; ;; (sum-array bigarray (length biglist))

;;   ;; (define dot-product (jit-get-function (env-lookup 'dot-product module-env)))
;;   ;; (define bigarray (list->cblock biglist _ulong))
;;   ;; (time (begin
;;   ;;         (dot-product bigarray bigarray (length biglist))
;;   ;;         (dot-product bigarray bigarray (length biglist))
;;   ;;         (dot-product bigarray bigarray (length biglist))
;;   ;;         (dot-product bigarray bigarray (length biglist))))
;;   ;; (time
;;   ;;  (begin(for/sum [(a1 (in-vector bigvec))
;;   ;;             (a2 (in-vector bigvec))]
;;   ;;          (unsafe-fx* a1 a2))
;;   ;;        (for/sum [(a1 (in-vector bigvec))
;;   ;;                  (a2 (in-vector bigvec))]
;;   ;;          (unsafe-fx* a1 a2))
;;   ;;        (for/sum [(a1 (in-vector bigvec))
;;   ;;                  (a2 (in-vector bigvec))]
;;   ;;          (unsafe-fx* a1 a2))
;;   ;;        (for/sum [(a1 (in-vector bigvec))
;;   ;;                  (a2 (in-vector bigvec))]
;;   ;;          (unsafe-fx* a1 a2))))
;;   ;; (define test (jit-get-function (env-lookup 'test module-env)))
;;   ;; (pretty-print (test))

;;   )


