#lang racket
(require ffi/unsafe)

(require "private/llvm/ffi/all.rkt")
(require "private/llvm/pass-table.rkt")
(require "private/env.rkt")
(require "private/types.rkt")
(require "private/internals.rkt")
(require "private/utils.rkt")

(provide (all-defined-out)
         env-lookup)

(define (llvm-initialize)
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
  (LLVMInitializeTarget gpr))
(llvm-initialize)

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
(define (jit-dump-function mod sym)
  (LLVMDumpValue (env-jit-function-ref (env-lookup sym mod))))

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

(define (jit-get-function-ptr f-sym mod)
  (define fptr (jit-compile-function f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (type-prim-racket (env-type-prim (env-jit-function-type fref))))
  fptr)

(define (jit-get-module mod-env)
  (env-lookup '#%jit-module mod-env))

(define (jit-run-function-pass passes jit-mod f)
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (for ([pass passes])
    ((jit-lookup-pass pass) fpm))
  (begin0
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref f))
    (LLVMDisposePassManager fpm)))

(define (jit-run-function-pass-env passes f-sym mod-env)
  (define jit-mod (jit-get-module mod-env))
  (jit-run-function-pass passes (env-lookup f-sym mod-env) jit-mod))

(define (jit-run-module-pass passes jit-mod)
  (define mpm (LLVMCreatePassManager))
  (for ([pass passes])
    (define module-pass (jit-lookup-pass pass))
    (module-pass mpm))
  (begin0
      (LLVMRunPassManager mpm jit-mod)
    (LLVMDisposePassManager mpm)))

(define (jit-run-module-pass-env passes mod-env)
  (define jit-mod (jit-get-module mod-env))
  (jit-run-module-pass passes jit-mod))

(define (jit-verify-module mod-env)
  (define jit-mod (env-lookup '#%jit-module mod-env))
  (LLVMVerifyModule jit-mod 'LLVMPrintMessageAction #f))

(define (jit-optimize-function mod-env #:opt-level [level 1])
  (define jit-mod (env-lookup '#%jit-module mod-env))
  (define fpm (LLVMCreateFunctionPassManagerForModule jit-mod))
  (define fpmb (LLVMPassManagerBuilderCreate))
  (LLVMPassManagerBuilderSetOptLevel fpmb level)
  (LLVMPassManagerBuilderPopulateFunctionPassManager fpmb fpm)
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
      (LLVMRunFunctionPassManager fpm (env-jit-function-ref (cdr m)))))
  (LLVMDisposePassManager fpm)
  (LLVMPassManagerBuilderDispose fpmb))

(define (jit-optimize-module mod-env #:opt-level [level 1])
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
  (for [(m mod-env)]
    (when (env-jit-function? (cdr m))
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
    (match stmt
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
                                        function-ref (symbol->string (gensym^ 'then))))
       (define else-block
         (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module)
                                        function-ref (symbol->string (gensym^ 'else))))
       
       (LLVMBuildCondBr jit-builder tst-value then-block else-block)

       (LLVMPositionBuilderAtEnd jit-builder then-block)
       (define thn-env (build-statement thn env))
       (define thn-end-blk (LLVMGetInsertBlock jit-builder))
       (define thn-return (env-contains? '#%return? thn-env))

       (LLVMPositionBuilderAtEnd jit-builder else-block)
       (define els-env (build-statement els env))
       (define els-end-blk (LLVMGetInsertBlock jit-builder))
       (define els-return (env-contains? '#%return? els-env))

       (if (and thn-return els-return)
           env
           (let ((end-block (LLVMAppendBasicBlockInContext
                             (LLVMGetModuleContext jit-module)
                             function-ref (symbol->string (gensym^ 'ifend)))))
             (unless thn-return
               (LLVMPositionBuilderAtEnd jit-builder thn-end-blk)
               (LLVMBuildBr jit-builder end-block))
             (unless els-return
               (LLVMPositionBuilderAtEnd jit-builder els-end-blk)
               (LLVMBuildBr jit-builder end-block))
             (LLVMPositionBuilderAtEnd jit-builder end-block)
             (for/fold [(env env)]
                       [(id (find-common-change thn-env els-env))]
               (define id-thn (env-lookup id thn-env))
               (define id-els (env-lookup id els-env))
               (define id-phi
                 (LLVMBuildPhi jit-builder
                               (type-prim-jit
                                (env-type-prim
                                 (env-jit-value-type
                                  id-thn)))
                               (symbol->string id)))
               (LLVMAddIncoming id-phi
                                (list (env-jit-value-ref id-thn)
                                      (env-jit-value-ref id-els))
                                (list thn-end-blk els-end-blk))
               (env-extend id (env-jit-value id-phi (env-jit-value-type id-thn)) env))))]

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
       (define body-end-blk (LLVMGetInsertBlock jit-builder))
       (for ([var vars]
             [phi tst-loop-phis])
         (LLVMAddIncoming phi
                          (list (env-jit-value-ref (env-lookup var env))
                                (env-jit-value-ref (env-lookup var body-env)))
                          (list prev-block body-end-blk)))
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
      [`(return-void)
       (LLVMBuildRetVoid jit-builder)
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
      [`(#%app ,rator ,rands ...);;TODO rator needs to be symbol
       (define rand-values
         (for/list ([rand rands])
           (build-expression rand env)))
       (build-app rator rand-values env)] ;;higher order functions
      [`(#%value ,value ,type) (build-value value type env)]
      [`(#%fl-value ,value ,type)
       (LLVMConstReal (type-prim-jit (env-type-prim (env-lookup type env)))
                      value)]
      [`(#%si-value ,value ,type)
       (LLVMConstInt (type-prim-jit (env-type-prim (env-lookup type env)))
                     value #t)]
      [`(#%ui-value ,value ,type)
       (LLVMConstInt (type-prim-jit (env-type-prim (env-lookup type env)))
                     value #f)]
      [`(#%sizeof ,type)
       (define envtype (env-lookup type env))
       (build-value (LLVMStoreSizeOfType jit-target-data
                                         (type-prim-jit (env-type-prim envtype)))
                    'i32 env)]
      [`(#%type ,t)
       (define envtype (env-lookup t env))
       (type-prim-jit (env-type-prim envtype))]
      ['#%void (void)]
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
    (match rator
      [`(#%jit-intr ,str-id ,ret-type)
       (define s (symbol->string str-id))
       (define fn-type
         (LLVMFunctionType (get-env-jit-type ret-type env)
                           (map LLVMTypeOf rand-values) #f))
       (define ref (LLVMAddFunction jit-module s fn-type))
       (LLVMBuildCall jit-builder ref rand-values (substring s 0 3))]
      [else
       (match (env-lookup rator env)
         [(env-jit-function ref type)
          (LLVMBuildCall jit-builder ref rand-values
                         (substring (symbol->string rator) 0 3))]
         [(env-jit-intr-function builder)
          (builder jit-builder rand-values)]
         [else (error "cannot figure out how to apply: ~a" rator)])]))
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
  ;; (define (diag-handler dinfo voidp)
  ;;   (printf "\tdiag: ~a\n" (LLVMGetDiagInfoDescription dinfo)))
  ;; (LLVMContextSetDiagnosticHandler context diag-handler #f)
  (define jit-module (LLVMModuleCreateWithNameInContext module-name context))
  (LLVMSetDataLayout jit-module "e-m:e-i64:64-f80:128-n8:16:32:64-S128")
  (LLVMSetTarget jit-module "x86_64-unknown-linux-gnu") ;TODO set target for machine
  (define jit-builder (LLVMCreateBuilderInContext context))

  (define (register-module-statement stmt env)
    (match stmt
      [`(define-type ,type-name ,t)
       (define type-decl (compile-type-declaration stmt env))
       (define type (compile-type-definition type-decl env))
       (env-extend type-name type env)]
      [`(define-function
          (#:pass ,p ...) ...
          (#:attr ,a ...) ...
          (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define type (create-type `(,@types -> ,ret-type) env))
       (define function-obj (compile-function-declaration type function-name jit-module))
       (env-extend function-name
        	   (env-jit-function function-obj type)
        	   env)]))

  (define (compile-module-statement stmt env module-env)
    ;; TODO return values of name and obj
    (match stmt
      [`(define-type ,type-name ,t)
       ;types are created in register phase; something for recursive struct types to be done
       ;; TODO for struct switch to creating empty struct type and then adding fields here
       (env-extend type-name (env-lookup type-name env) module-env)]
      [`(define-function
          (#:pass ,passes ...) ...
          (#:attr ,attrs ...) ...
          (,function-name (,args : ,types) ... : ,ret-type) ,body)
       (define f (compile-function-definition function-name args types ret-type
        				      body (env-lookup function-name env)
        				      env jit-module jit-builder))

       (for ([attr (flatten attrs)])
         (LLVMAddAttributeAtIndex
          (env-jit-function-ref f)
          (cast -1 _int _uint)
          (jit-lookup-attr attr context)))
       (jit-run-function-pass (flatten passes) jit-module f)
       (env-extend function-name f module-env)]))
  (match m
    [`(#%module (#:pass ,passes ...) ...
                ,module-stmts ...)
     (define env
       (for/fold ([env (create-initial-environment context)])
                 ([stmt module-stmts])
         (register-module-statement stmt env)))
     (define module-env
       (for/fold ([module-env (empty-env)])
                ([stmt module-stmts])
         (compile-module-statement stmt env module-env)))
     (jit-run-module-pass (flatten passes) jit-module)
     ;; (LLVMVerifyModule jit-module 'LLVMPrintMessageAction #f)
     (env-extend '#%jit-module jit-module module-env)]))


(module+ test
  (require racket/unsafe/ops)
  (require rackunit)
  ;; (require "../disassemble/disassemble/main.rkt")

  (define module-env
    (compile-module
     '(#%module
       (#:pass AlwaysInliner)
       (define-type int i32)
       (define-type int* (pointer int))

       (define-type bc (struct (b : i32) (c : i32)))
       (define-type pbc (pointer bc))
       (define-type ui (struct (bc1 : pbc) (bc2 : pbc)))

       (define-function (id (x : int) : int)
         (return x))

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

       (define-function (#:attr AlwaysInline) (add (x : int) (y : int) : int)
         (return (#%app jit-add x y)))

       (define-function (sum-array (arr : int*) (size : int) : int)
         (let ((i : int (#%value 0 int))
               (sum : int (#%value 0 int)))
           (block
            (while ((sum : int) (i : int))
              (#%app jit-icmp-ult i size)
              (block
               (let ((arri : int* (#%gep arr (i))))
                 (set! sum (#%app add
                                  sum
                                  (#%app jit-load arri))))
               (set! i (#%app jit-add i (#%value 1 int)))))
            (return sum)))))))

  (jit-dump-module module-env)
  (jit-optimize-module module-env #:opt-level 3)
  (jit-dump-module module-env)
  (error 'stop)
  (jit-verify-module module-env)
  (define cenv (initialize-jit module-env #:opt-level 3))

  (define add (jit-get-function 'add cenv))

  (define factr (jit-get-function 'factr cenv))
  (define factc (jit-get-function 'factc cenv))
  (define fact (jit-get-function 'fact cenv))
  (define even? (jit-get-function 'even? cenv))
  (define meven? (jit-get-function 'meven? cenv))
  ;; (disassemble-ffi-function (jit-get-function-ptr 'sum-array cenv)
  ;;                           #:size 70)

  (check-eq? (add 3 5) 8)
  (check-eq? (fact 5) 120)
  (check-eq? (factr 5) 120)
  (check-eq? (factc 5) 120)
  (check-eq? (even? 42) 1)
  (check-eq? (meven? 21) 0)

  (define malloc-test (jit-get-function 'malloc-test cenv))
  (check-eq? (malloc-test) 42)

  (define sum-array (jit-get-function 'sum-array cenv))
  (check-eq? (sum-array (list->cblock '(1 2 3) _uint) 3) 6))
