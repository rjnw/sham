#lang racket
(require ffi/unsafe)

(require "llvm/ffi/all.rkt")

(require "jit-env.rkt")
(require "jit-type.rkt")
(require "jit-intr.rkt")
(require "jit-expand.rkt")

(provide (all-defined-out)
         env-lookup)

;; (provide compile-module
;;          create-jit-context
;;          create-initial-environment
;;          jit-get-function
;;          jit-dump-function
;;          jit-dump-module
;;          jit-get-racket-type
;;          env-lookup
;;          context)

(LLVMLinkInMCJIT)
(LLVMInitializeX86Target)
(LLVMInitializeX86TargetInfo)
(LLVMInitializeX86TargetMC)
(LLVMInitializeX86AsmParser)
(LLVMInitializeX86AsmPrinter)
(define gpr (LLVMGetGlobalPassRegistry))
(LLVMInitializeCore gpr)
(LLVMInitializeTransformUtils gpr)
(LLVMInitializeScalarOpts gpr)
(LLVMInitializeObjCARCOpts gpr)
(LLVMInitializeVectorization gpr)
(LLVMInitializeInstCombine gpr)
(LLVMInitializeIPO gpr)
(LLVMInitializeInstrumentation gpr)
(LLVMInitializeAnalysis gpr)
(LLVMInitializeIPA gpr)
(LLVMInitializeCodeGen gpr)
(LLVMInitializeTarget gpr)

(define (create-jit-context)
  (LLVMContextCreate))
(define global-jit-context (LLVMGetGlobalContext))

(define (create-initial-environment context)
  (register-jit-internals (register-initial-types (empty-env) context) context))

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
      (error "error initializing jit" status err)
      (env-extend '#%mcjit-engine engine mod)))

;; TODO check for mcjit-engine in env
(define (jit-compile-function f-sym mod)
  (define mcjit-engine (env-lookup '#%mcjit-engine mod))
  (cast (LLVMGetFunctionAddress mcjit-engine (symbol->string f-sym)) _uint64 _pointer))

(define (jit-get-function f-sym mod)
  (define fptr (jit-compile-function f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (type-prim-racket (env-type-prim (env-jit-function-type fref))))
  (cast fptr _pointer f-type))

(define (jit-optimize-function f-sym mod-env)
  (define jit-mod (env-lookup '#%jit-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb 3)
  (LLVMPassManagerBuilderSetDisableUnrollLoops fpmb #t)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (LLVMRunFunctionPassManager fpm (env-lookup f-sym mod-env)))

(define (jit-optimize-all mod-env)
  (define jit-mod (env-lookup '#%jit-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb 3)
  (LLVMPassManagerBuilderSetDisableUnrollLoops fpmb #t)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (printf "optimizing ~a\n" (car m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m))))))

(define (jit-optimize-module mod-env [level 1])
  (define jit-mod (env-lookup '#%jit-module mod-env))
  (define mpm (LLVMCreatePassManager))
  (define pmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel pmb level)
  (LLVMPassManagerBuilderSetSizeLevel pmb 100)
  (LLVMPassManagerBuilderPopulateModulePassManager pmb mpm)
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)
    (LLVMPassManagerBuilderDispose pmb)))

(define (jit-run-basic-pass mod-env)
  (define jit-mod (env-lookup '#%jit-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (LLVMAddCFGSimplificationPass fpm)
  ;; (LLVMAddGVNPass fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (printf "optimizing ~a\n" (car m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm))

(define (jit-get-racket-type t)
  (type-prim-racket (env-type-prim t)))

(define (get-env-jit-type t env)
  (type-prim-jit
   (env-type-prim
    (env-lookup t env))))

;; returns new env  
(define (compile-statement stmt function-ref env jit-module jit-builder)
  (define jit-target-data (LLVMCreateTargetData (LLVMGetDataLayout jit-module)))
  (define (build-statement stmt env)
    (define (find-common-change env1 env2)
      (define env1-ids (list->set (map car env1)))
      (define env2-ids (list->set (map car env2)))
      (for/set [(id (set-intersect env1-ids env2-ids))
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
           (env-extend id
                       (env-jit-value (build-expression val env) id-type)
                       env)))
       (build-statement st new-env)]

      [`(set! ,lhs ,v)
       (define v-ref (build-expression v env))
       (define lhs-v (env-lookup lhs env))
       (env-extend lhs (env-jit-value v-ref (env-jit-value-type lhs-v)) env)]

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
       (define thn-return (env-contains? '#%return? thn-env))
       (unless thn-return
         (LLVMBuildBr jit-builder end-block))

       (LLVMPositionBuilderAtEnd jit-builder else-block)
       (define els-env (build-statement els env))
       (define els-return (env-contains? '#%return? els-env))
       (unless els-return (LLVMBuildBr jit-builder end-block))

       (define both-return (and thn-return els-return))
       (if both-return
           env
           (begin
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
               (env-extend id id-phi env))))]

      [`(while ((,vars : ,var-types) ...) ,tst ,body)

       (define prev-block (LLVMGetInsertBlock jit-builder))
       (define loop-entry
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "loop-entry"))
       (define loop-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "loop-block"))
       (define afterloop-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref "afterloop-block"))

       (LLVMBuildBr jit-builder loop-entry)

       (LLVMPositionBuilderAtEnd jit-builder loop-entry)
       (define tst-loop-phis
         (for/list
             ([var vars]
              [type var-types])
           (LLVMBuildPhi jit-builder
                         (type-prim-jit (env-type-prim (env-lookup type env)))
                         (symbol->string var))))
       (define tst-env (for/fold
                           [(env env)]
                           [(var vars)
                            (var-ref tst-loop-phis)
                            (type var-types)]
                         (env-extend var
                                     (env-jit-value var-ref
                                                    (env-lookup type env))
                                     env)))
       (define tst-value (build-expression tst tst-env))
       (LLVMBuildCondBr jit-builder tst-value loop-block afterloop-block)

       (LLVMPositionBuilderAtEnd jit-builder loop-block)

       (define body-loop-phis
         (for/list
             ([var vars]
              [type var-types])
           (LLVMBuildPhi jit-builder
                         (type-prim-jit (env-type-prim (env-lookup type env)))
                         (symbol->string var))))
       (define new-body-env
         (for/fold
             [(env env)]
             [(var vars)
              (var-ref body-loop-phis)
              (type var-types)]
           (env-extend var
                       (env-jit-value var-ref
                                      (env-lookup type env))
                       env)))
       
       (for ([var vars]
             [phi body-loop-phis])
         (LLVMAddIncoming phi
                          (list (env-jit-value-ref (env-lookup var tst-env)))
                          (list loop-entry)))
       
       (define body-env (build-statement body new-body-env))
       (for ([var vars]
             [phi tst-loop-phis])
         (LLVMAddIncoming phi
                          (list (env-jit-value-ref (env-lookup var env))
                                (env-jit-value-ref (env-lookup var body-env)))
                          (list prev-block loop-block)))
       (LLVMBuildBr jit-builder loop-entry)

       (LLVMPositionBuilderAtEnd jit-builder afterloop-block)

       (for/fold ([env env])
                 ([id (find-common-change env body-env)])
         (define id-phi
           (LLVMBuildPhi jit-builder
                         (type-prim-jit
                          (env-type-prim
                           (env-jit-value-type
                            (env-lookup id body-env))))
                         (symbol->string id)))
         (LLVMAddIncoming id-phi
                          (list (env-jit-value-ref (env-lookup id tst-env)))
                          (list loop-entry))
         (env-extend id
                     (env-jit-value id-phi
                                    (env-jit-value-type
                                     (env-lookup id body-env)))
                     env))]

      [`(return ,v)
       (LLVMBuildRet jit-builder (build-expression v env))
       (env-extend '#%return? #t env)]

      [`(block ,stmts ...)
       (for/fold ([env env])
                 ([stmt stmts])
         (build-statement stmt env))]

      [`(#%exp ,e)
       (build-expression e env)
       env]
      [else (error "unknown statement or not implemented" stmt)]))
  
  (define (build-expression e env)
    (match e
      [`(#%app ,rator ,rands ...);;rator needs to be symbol
       (define rand-values
         (for/list ([rand rands])
           (build-expression rand env)))
       (build-app rator rand-values env)] ;;higher order functions
      [`(#%value ,value ,type) (build-value value type env)]
      [`(#%sizeof ,type)
       (define envtype (env-lookup type env))
       (build-value (LLVMStoreSizeOfType jit-target-data
                                         (type-prim-jit (env-type-prim envtype)))
                    'i32 env)]
      [`(#%type ,t)
       (define envtype (env-lookup t env))
       (type-prim-jit (env-type-prim envtype))]
      [`(#%gep ,ptr ,indxs)
       (LLVMBuildGEP jit-builder (build-expression ptr env)
                     (map (curryr build-expression env)
                          indxs)
                     "gep")]
      [(? symbol?) (env-jit-value-ref (env-lookup e env))]))
  
  (define (build-value value type env)
    (define envtype (env-lookup type env))
    (define prim-type (type-prim-jit (env-type-prim envtype)))
    (cond [(or (type-native-int? envtype)
               (type-pointer? (env-type-skel envtype)))
           (LLVMConstInt prim-type value #f)] ;;add signed
          [(type-float32? envtype)
           (LLVMConstReal prim-type value)]
          [else (error "value type not supported yet!" value type)]))
  (define (build-app rator rand-values env)
    (match (env-lookup rator env)
      [(env-jit-function ref type)
       (LLVMBuildCall jit-builder ref rand-values (symbol->string rator))]
      [(env-jit-intr-function builder)
       (builder jit-builder rand-values)]
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
  (LLVMSetTarget jit-module "x86_64-unknown-linux-gnu")
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
     (LLVMVerifyModule jit-module 'LLVMPrintMessageAction #f)
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

       (define-function (const1 : int)
         (return (#%value 1 int)))
       (define-function (even? (x : int) : int)
         (if (#%app jit-icmp-eq (#%app jit-urem x (#%value 2 int)) (#%value 0 int))
             (return (#%value 1 int))
             (return (#%value 0 int))))

       (define-function (meven? (x : int) : int)
         (if (#%app jit-icmp-eq x (#%value 0 int))
             (return (#%value 1 int))
             (return (#%app modd? (#%app jit-sub x (#%value 1 int))))))

       (define-function (modd? (x : int) : int)
         (if (#%app jit-icmp-eq x (#%value 0 int))
             (return (#%value 0 int))
             (return (#%app meven?
                            (#%app jit-sub x (#%value 1 int))))))

       (define-function (fact (x : int) : int)
         (if (#%app jit-icmp-eq x (#%value 0 int))
             (return (#%value 1 int))
             (return (#%app jit-mul
                            x
                            (#%app fact
                                   (#%app jit-sub x (#%value 1 int)))))))

       (define-type int* (pointer int))
       (define-function (factc (x : int) : int)
         (let ((result* : int* (#%app jit-alloca (#%type int)))
               (i* : int* (#%app jit-alloca (#%type int)))
               (x* : int* (#%app jit-alloca (#%type int))))
           (block
            (#%exp (#%app jit-store! x x*))
            (#%exp (#%app jit-store! (#%value 1 int) result*))
            (#%exp (#%app jit-store! (#%value 1 int) i*))
            (while () (#%app jit-icmp-ule
                             (#%app jit-load i*)
                             (#%app jit-load x*))
                   (block
                    (#%exp (#%app jit-store! 
                                  (#%app jit-mul-nuw (#%app jit-load result*)
                                         (#%app jit-load i*))
                                  result*))
                    (#%exp (#%app jit-store!
                            (#%app jit-add-nuw (#%app jit-load i*)
                                   (#%value 1 int))
                            i*))))
            (return (#%app jit-load result*)))))


       (define-function (factr (x : int) : int)
         (let ((result : int (#%value 1 int)))
           (let ((i : int (#%value 1 int)))
             (block
              (while ((result : int) (i : int))
                (#%app jit-icmp-ule i x)
                (block
                 (set! result (#%app jit-mul-nuw result i))
                 (set! i (#%app jit-add-nuw i (#%value 1 int)))))
              (return result)))))



       (define-function (malloc-test  : int)
         (let ((ptr : void* (#%app jit-malloc (#%type int))))
           (block
            (#%exp (#%app jit-store! (#%value 42 int) ptr ))
            (return (#%app jit-load ptr)))))


       (define-function (sum-array (arr : int*) (size : int) : int)
         (let ((i : int (#%value 0 int))
               (sum : int (#%value 0 int)))
           (block
            (while ((sum : int) (i : int))
              (#%app jit-icmp-ult i size)
              (block
               (let ((arri : int* (#%gep arr (i))))
                 (set! sum (#%app jit-add
                                  sum
                                  (#%app jit-load arri))))
               (set! i (#%app jit-add i (#%value 1 int)))))
            (return sum))))


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
       
       )))

  (jit-dump-module module-env)
  (define cenv (initialize-jit module-env))

  (define add (jit-get-function 'add cenv))
  (define fact (jit-get-function 'fact cenv))
  (define factr (jit-get-function 'factr cenv))
  (define factc (jit-get-function 'factc cenv))
  (define even? (jit-get-function 'even? cenv))
  (define meven? (jit-get-function 'meven? cenv))

  (check-eq? (add 3 5) 8)
  (check-eq? (fact 5) 120)
  (check-eq? (factr 5) 120)
  (check-eq? (factc 5) 120)
  (check-eq? (even? 42) 1)
  (check-eq? (meven? 21) 0)

  (define malloc-test (jit-get-function 'malloc-test cenv))
  (check-eq? (malloc-test) 42)

  (define sum-array (jit-get-function 'sum-array cenv))
  (check-eq? (sum-array (list->cblock '(1 2 3) _uint) 3) 6)
  

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


