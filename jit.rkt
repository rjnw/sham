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

(define (create-initial-environment context)
  (register-initial-types (empty-env) context))

(define (jit-dump-module mod)
  (for [(m mod)]
    (let ([s (car m)]
          [v (cdr m)])
      (cond ([env-type? v]
             (begin
               (printf "type ~a: ~a\n" s
                       (LLVMPrintTypeToString (type-prim-jit (env-type-prim v))))))
            ([env-jit-function? v]
             (LLVMDumpValue (env-jit-function-ref v)))))))

(define (initialize-jit mod #:opt-level [opt-level 1])
  (define mcjit-options (LLVMInitializeMCJITCompilerOptions))
  (set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options opt-level)
  (define-values (engine status err)
    (LLVMCreateMCJITCompilerForModule (env-lookup '#%jit-module mod)
                                      mcjit-options))
  (if status
      (error "error initializing jit" status)
      (env-extend '#%mcjit-engine engine mod)))

;; TODO check for mcjit-engine in env
(define (jit-compile-function f-sym mod)
  (define mcjit-engine (env-lookup '#%mcjit-engine mod))
  (LLVMGetFunctionAddress mcjit-engine (symbol->string f-sym)))

(define (jit-get-racket-type t)
  (type-prim-racket (env-type-prim t)))

(define (get-env-jit-type t env)
  (type-prim-jit
   (env-type-prim
    (env-lookup t env))))

;; returns new env  
(define (compile-statement stmt function-ref env jit-module jit-builder)
  (define (build-statement stmt env)
    (define (find-common-change env1 env2)
      (define env1-ids (list->set (map car env1)))
      (define env2-ids (list->set (map car env2)))
      (for/list [(id (set-intersect env1-ids env2-ids))
                 #:when (not (eq? (env-lookup id env1)
                                  (env-lookup id env2)))]
        id))
    (define (find-different env1 env2)
      (define env1-ids (list->set (map car env1)))
      (define env2-ids (list->set (map car env2)))
      (set-subtract env2-ids env1-ids))
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
       (define v-ref (build-expression v env))
       (define lhs-v (env-lookup lhs env))
       (env-extend lhs (env-jit-value v-ref (env-jit-value-type lhs-v)))]

      [`(store! ,lhs ,v)
       (LLVMBuildStore jit-builder
                       (env-jit-value-ref (env-lookup lhs env))
                       (build-expression v env))]

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
       (define thn-env (build-statement thn env))
       (LLVMBuildBr jit-builder end-block)

       (LLVMPositionBuilderAtEnd jit-builder else-block)
       (define els-env (build-statement els env))
       (LLVMBuildBr jit-builder end-block)

       (LLVMPositionBuilderAtEnd jit-builder end-block)
       (for/fold [(env env)]
                 [(id (find-common-change thn-env els-env))]
         (define id-phi
           (LLVMBuildPhi jit-builder
                         (type-prim-jit
                          (env-type-prim
                           (env-jit-value-type
                            (env-lookup id thn-env))))
                         (symbol->string id)))
         (LLVMAddIncoming id-phi
                          (list (env-lookup id thn-env) (env-lookup id els-env))
                          (list then-block else-block))
         (env-extend id id-phi env))]

      [`(while ((,vars : ,var-types) ...) ,tst ,body)
       (define tst-value (build-expression tst env))
       (define loop-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "loop-block"))
       (define afterloop-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "afterloop-block"))

       (LLVMBuildCondBr jit-builder tst-value loop-block afterloop-block)

       (LLVMPositionBuilderAtEnd jit-builder loop-block)
       (define loop-phis
         (for/list
             ([var vars]
              [type var-types])
           (LLVMBuildPhi jit-builder
                         (type-prim-jit (env-type-prim (env-lookup type env)))
                         (symbol->string var))))
       (define new-env (for/fold
                           [(env env)]
                           [(var vars)
                            (var-ref loop-phis)
                            (type var-types)]
                         (env-extend var (env-jit-value var-ref (env-lookup type env)) env)))
       (define body-env (build-statement body env))
       (for ([var vars]
             [phi loop-phis])
         (LLVMAddIncoming phi
                          (list (env-jit-value-ref (env-lookup var body-env)))
                          (list loop-block)))
       (LLVMBuildBr jit-builder afterloop-block)

       (LLVMPositionBuilderAtEnd jit-builder afterloop-block)
       (for/fold ([env env])
                 ([id (find-different env body-env)])
         (define id-phi
           (LLVMBuildPhi jit-builder
                         (type-prim-jit
                          (env-type-prim
                           (env-jit-value-type
                            (env-lookup id body-env))))
                         (symbol->string id)))
         (LLVMAddIncoming id-phi
                          (list (env-lookup id body-env))
                          (list loop-block))
         (env-extend id id-phi env))]

      [`(return ,v)
       (LLVMBuildRet jit-builder (build-expression v env))]

      [`(block ,stmts ...)
       (for/fold ([env env])
                 ([stmt stmts])
         (build-statement stmt env))]

      [`(#%exp ,e)
       (build-expression e env)]
      [else (error "unknown statement or not implemented" stmt)]))
  
  (define (build-expression e env)
    (match e
      [`(#%app ,rator ,rands ...);;rator needs to be symbol
       (define rand-values
         (for/list ([rand rands])
           (build-expression rand env)))
       (build-app rator rand-values env)] ;;higher order functions
      [`(#%value ,value ,type) (build-value value type env)]
      [`(#%load ,ptr) (LLVMBuildLoad jit-builder (build-expression ptr env) "ld")]
      [`(#%sizeof ,type)
       (define envtype (env-lookup type env))
       (build-value (LLVMStoreSizeOfType (type-prim-jit (env-type-prim envtype)))
                     'i32 env)]
      [`(#%gep ,ptr ,indxs) 
       (LLVMBuildGEP jit-builder (build-expression ptr env)
                     (map (curryr build-expression indxs env))
                     "gep")]
      [(? symbol?) (env-jit-value-ref (env-lookup e env))]))
  
  (define (build-value value type env)
    (define envtype (env-lookup type env))
    (define prim-type (type-prim-jit (env-type-prim envtype)))
    (cond [(or (type-native-int? envtype)
               (type-pointer? (env-type-skel envtype)))
           (env-jit-value (LLVMConstInt jit-builder prim-type #f) envtype)] ;;add signed
          [(type-float32? envtype)
           (env-jit-value (LLVMConstReal prim-type value) envtype)]
          [else (error "value type not supported yet!" value type)]))
  (define (build-app rator rand-values function env)
    (match (env-lookup rator env)
      [(env-jit-function ref type)
       (LLVMBuildCall jit-builder ref rand-values (symbol->string rator))]
      [(env-racket-function type f)
       ;TODO
       (error "not implemented applicative")]
      [(env-racket-ffi-function type f)
       ;TODO
       (error "not implemented applicative")]
      [else (error "rator ~a\n" (env-lookup rator env))]))
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
  (LLVMPositionBuilderAtEnd jit-builder entry-block)
  (compile-statement body fref new-env jit-module jit-builder)
  env-function)

(define (compile-function-declaration env-function-type function-name jit-module)
  (define function-type (type-prim-jit (env-type-prim env-function-type)))
  (LLVMAddFunction jit-module (symbol->string function-name) function-type))

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
       (define function-obj (compile-function-declaration type function-name jit-module))
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
  (match m
    [`(#%module ,module-stmts ...)
     (define env
       (for/fold ([env (create-initial-environment context)])
                 ([stmt module-stmts])
         (register-module-statement stmt env)))
     (define module-env
       (for/fold ([module-env (empty-env)])
                ([stmt module-stmts])
         (compile-module-statement stmt env module-env)))
     (env-extend '#%jit-module jit-module module-env)]))


(module+ test
  (require racket/unsafe/ops)
  (require rackunit)
  (define module-env
    (compile-module
     '(#%module
       (define-type int i32)
       (define-type bc (struct (b : i32) (c : i32)))
       (define-type pbc (pointer bc))
       (define-type ui (struct (bc1 : pbc) (bc2 : pbc)))

       (define-function (id (x : int) : int)
         (return x))

       (define-function (add (x : int) (y : int) : int)
         (return (#%app jit-add x y)))

       ;; (define-function (even? (x : int) : int)
       ;;   (if (#%app jit-eq? (#%app jit-rem x (#%value 2 int)) (#%value 0 int))
       ;;       (return (#%value 1 int))
       ;;       (return (#%value 0 int))))

       ;; (define-function (meven? (x : int) : int)
       ;;   (if (#%app jit-eq? x (#%value 0 int))
       ;;       (return (#%value 1 int))
       ;;       (return (#%app modd? (#%app jit-sub x (#%value 1 int))))))

       ;; (define-function (modd? (x : int) : int)
       ;;   (if (#%app jit-eq? x (#%value 0 int))
       ;;       (return (#%value 0 int))
       ;;       (return (#%app meven?
       ;;                      (#%app jit-sub x (#%value 1 int))))))

       ;; (define-function (fact (x : int) : int)
       ;;   (if (#%app jit-eq? x (#%value 0 int))
       ;;       (return (#%value 1 int))
       ;;       (return (#%app jit-mul
       ;;                      x
       ;;                      (#%app fact
       ;;                             (#%app jit-sub x (#%value 1 int)))))))

       ;; (define-function (factr (x : int) : int)
       ;;   (let ((result : int))
       ;;     (block
       ;;      (set! result (#%value 1 int))
       ;;      (let ((i : int))
       ;;        (block
       ;;         (set! i (#%value 1 int))
       ;;         (while (#%app jit-le? i x)
       ;;           (block
       ;;            (set! result (#%app jit-mul result i))
       ;;            (set! i (#%app jit-add i (#%value 1 int)))))
       ;;         (return result))))))

       ;; (define-function (malloc-test  : int)
       ;;   (let ((ptr : void*))
       ;;     (let ((x : int))
       ;;       (block
       ;;        (set! ptr (#%app jit-malloc (#%sizeof int)))
       ;;        (set! (* ptr (#%value 0 int) : int) (#%value 8 int))
       ;;        (set! x (* ptr (#%value 0 int) : int))
       ;;        (#%exp (#%app jit-free ptr))
       ;;        (return x)))))
       ;; (define-type int* (pointer int))

       ;; (define-function (sum-array (arr : int*) (size : int) : int)
       ;;   (let ((i : int))
       ;;     (let ((sum : int))
       ;;       (block
       ;;        (set! i (#%value 0 int))
       ;;        (set! sum (#%value 0 int))
       ;;        (while (#%app jit-lt? i size)
       ;;          (block
       ;;           (set! sum (#%app jit-add
       ;;                            sum
       ;;                            (* arr (#%app jit-mul i (#%sizeof int)) : int)))
       ;;           (set! i (#%app jit-add i (#%value 1 int)))))
       ;;        (return sum)))))

       ;; (define-type ulong* (pointer ulong))
       ;; (define-type uint* (pointer uint))

       ;; (define-function (dot-product (arr1 : uint*) (arr2 : uint*)
       ;;                               (size : uint) : uint)
       ;;   (let ((sum : uint)
       ;;         (i : uint))
       ;;     (block
       ;;      (set! i (#%value 0 uint))
       ;;      (set! sum (#%value 0 uint))
       ;;      (while (#%app jit-lt? i size)
       ;;        (let ((ptr-pos : int))
       ;;          (block
       ;;           (set! ptr-pos (#%app jit-mul i (#%sizeof uint)))
       ;;           (set! sum (#%app jit-add
       ;;                            sum
       ;;                            (#%app jit-mul
       ;;                                   (* arr1 ptr-pos : uint)
       ;;                                   (* arr2 ptr-pos : uint))))
       ;;           (set! i (#%app jit-add i (#%value 1 uint))))))
       ;;      (return sum))))
       ;; (define-type float32-p (pointer float32))
       ;; (define-type array-real (struct (size : int) (data : float32-p)))
       ;; (define-type array-real-p (pointer array-real))
       ;; (define-function (get-size (arr : array-real-p) : int)
       ;;   (return (* arr (#%offset array-real size) : int)))
       
       ;; (define-function (test : int)
       ;;   (let ((ap : array-real-p))
       ;;     (block (set! ap (#%app jit-malloc (#%sizeof array-real)))
       ;;            (set! (* ap (#%offset array-real size) : int) (#%value 42 int))
       ;;            (return (#%app get-size ap)))))
       )))

  (jit-dump-module module-env)
  ;; (jit-dump-function (env-lookup 'fact module-env)
  ;;                     "/tmp/jitdump" "fact")
  ;; (pretty-print (f 21))
  ;; (define even? (jit-get-function (env-lookup 'even? module-env)))
  ;; (pretty-print (even? 42))
  ;; (define meven? (jit-get-function (env-lookup 'meven? module-env)))
  ;; (pretty-print (meven? 21))

  ;; (define fact (jit-get-function (env-lookup 'fact module-env)))
  ;; (pretty-print (fact 5))
  ;; (define factr (jit-get-function (env-lookup 'factr module-env)))
  ;; (pretty-print (factr 5))
  ;; (define malloc-test (jit-get-function (env-lookup 'malloc-test module-env)))
  ;; (malloc-test)
  ;; (define sum-array (jit-get-function (env-lookup 'sum-array module-env)))
  ;; (define biglist (stream->list (in-range 1000000)))
  
  ;; (define bigvec (for/vector ([i (in-range 1000000)])
  ;;                  i))
  ;; ;; (sum-array bigarray (length biglist))

  ;; (define dot-product (jit-get-function (env-lookup 'dot-product module-env)))
  ;; (define bigarray (list->cblock biglist _ulong))
  ;; (time (begin
  ;;         (dot-product bigarray bigarray (length biglist))
  ;;         (dot-product bigarray bigarray (length biglist))
  ;;         (dot-product bigarray bigarray (length biglist))
  ;;         (dot-product bigarray bigarray (length biglist))))
  ;; (time
  ;;  (begin(for/sum [(a1 (in-vector bigvec))
  ;;             (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))
  ;;        (for/sum [(a1 (in-vector bigvec))
  ;;                  (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))
  ;;        (for/sum [(a1 (in-vector bigvec))
  ;;                  (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))
  ;;        (for/sum [(a1 (in-vector bigvec))
  ;;                  (a2 (in-vector bigvec))]
  ;;          (unsafe-fx* a1 a2))))
  ;; (define test (jit-get-function (env-lookup 'test module-env)))
  ;; (pretty-print (test))

  )


