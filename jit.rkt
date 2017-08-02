#lang racket
(require ffi/unsafe)

(require "private/llvm/ffi/all.rkt")
(require "private/llvm/pass-table.rkt")
(require "private/env.rkt")
(require "private/types.rkt")
(require "private/ast.rkt")
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
                       (LLVMPrintTypeToString (internal-type-jit (env-type-prim v))))))
            ([env-jit-function? v]
             (LLVMDumpValue (env-jit-function-ref v)))))))
(define (jit-dump-function mod sym)
  (LLVMDumpValue (env-jit-function-ref (env-lookup sym mod))))

(define (jit-write-bitcode mod fname)
  (LLVMWriteBitcodeToFile (env-lookup '#%jit-module mod) fname))

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
  (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
  (cast fptr _pointer f-type))

(define (jit-get-function-ptr f-sym mod)
  (define fptr (jit-compile-function f-sym mod))
  (define fref (env-lookup f-sym mod))
  (define f-type (internal-type-racket (env-type-prim (env-jit-function-type fref))))
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
  (internal-type-racket (env-type-prim t)))

(define (compile-function-definition name args types ret-type
				     body env-function env jit-module jit-builder)
  (define fref (env-jit-function-ref env-function))
  (define ftype (env-jit-function-type env-function))
  (define jit-target-data (LLVMCreateTargetData (LLVMGetDataLayout jit-module)))
  (define (build-llvm-type t env)
    (internal-type-jit (compile-type t env)))
  (define (new-block n)
    (LLVMAppendBasicBlockInContext (LLVMGetModuleContext jit-module) fref (symbol->string n)))
  (define (build-statement stmt env)
    (match stmt
      [(sham:stmt:let ids types vals st)
       (define id-types (map (curryr env-lookup env) types))
       (define new-env
         (for/fold ([env env])
                   ([id-type id-types]
                    [id ids]
                    [val vals])
           (define p (LLVMBuildAlloca jit-builder (build-llvm-type id-type env)
                                      (symbol->string id)))
           (env-extend id p env)
           (unless (sham:exp:void-value? val)
             (LLVMBuildStore jit-builder p (build-expression val)))))
       (build-statement st new-env)]

      [(sham:stmt:set! lhs v)
       (define v-ref (build-expression v env))
       (define lhs-v (env-lookup lhs env))
       (LLVMBuildStore jit-builder
                       (env-jit-value v-ref (env-jit-value-type lhs-v))
                       lhs-v)]

      [(sham:stmt:if tst thn els)
       (define tst-value (build-expression tst env))
       (define then-block (new-block 'then))
       (define else-block (new-block 'else))
       
       (LLVMBuildCondBr jit-builder tst-value then-block else-block)

       (LLVMPositionBuilderAtEnd jit-builder then-block)
       (build-statement thn env)

       (LLVMPositionBuilderAtEnd jit-builder else-block)
       (build-statement els env)]

      [(sham:stmt:while tst body)

       (define prev-block (LLVMGetInsertBlock jit-builder))
       (define loop-entry (new-block 'loop-entry))
       (define loop-block (new-block 'loop-block))
       (define afterloop-block (new-block 'afterloop-block))

       (LLVMBuildBr jit-builder loop-entry)

       (LLVMPositionBuilderAtEnd jit-builder loop-entry)
       (define tst-value (build-expression tst env))
       (LLVMBuildCondBr jit-builder tst-value loop-block afterloop-block)

       (LLVMPositionBuilderAtEnd jit-builder loop-block)

       (build-statement body env)
       (define body-end-blk (LLVMGetInsertBlock jit-builder))
       (LLVMBuildBr jit-builder loop-entry)

       (LLVMPositionBuilderAtEnd jit-builder afterloop-block)]
      [(sham:stmt:return v)
       (LLVMBuildRet jit-builder (build-expression v env))]
      [(sham:stmt:return-void)
       (LLVMBuildRetVoid jit-builder)]
      [(sham:stmt:block stmts)
       (for ([stmt stmts])
         (build-statement stmt env))]
      [(sham:stmt:exp e)
       (build-expression e env)]
      [else (error "unknown statement" stmt)]))
  
  (define (build-expression e env)
    (match e
      [(sham:exp:app rator rands)
       (define rand-values (for/list ([rand rands]) (build-expression rand env)))
       (build-app rator rand-values env)]
      [(sham:exp:fl-value value t) (LLVMConstReal (build-llvm-type t env) value)]
      [(sham:exp:si-value value t) (LLVMConstInt (build-llvm-type t env) value #t)]
      [(sham:exp:ui-value value t) (LLVMConstInt (build-llvm-type t env) value #f)]
      [(sham:exp:sizeof type)
       (define envtype (env-lookup type env))
       (LLVMConstInt (internal-type-jit (env-type-prim (env-lookup 'i32 env)))
                     (LLVMStoreSizeOfType jit-target-data
                                          (internal-type-jit (env-type-prim envtype)))
                     #f)]
      [(sham:exp:type t)
       (define envtype (env-lookup t env))
       (internal-type-jit (env-type-prim envtype))]
      [(sham:exp:void-value) (void)]
      [(sham:exp:gep ptr indxs)
       (LLVMBuildGEP jit-builder
                     (build-expression ptr env)
                     (map (curryr build-expression env) indxs)
                     "gep")]
      [(sham:exp:var sym)
       (LLVMBuildLoad jit-builder
                      (env-jit-value-ref (env-lookup sym env))
                      (symbol->string sym))]))
  
  (define (build-app rator rand-values env)
    (match rator
      [(sham:rator:intrinsic str-id ret-type)
       (define s (symbol->string str-id))
       (define fn-type (LLVMFunctionType (build-llvm-type ret-type env) (map LLVMTypeOf rand-values) #f))
       (define ref (LLVMAddFunction jit-module s fn-type))
       (LLVMBuildCall jit-builder ref rand-values (substring s 0 3))]
      [(sham:rator:symbol sym)
       (match (env-lookup sym env)
         [(env-jit-function ref type)
          (LLVMBuildCall jit-builder ref rand-values (substring (symbol->string sym) 0 3))]
         [(env-jit-intr-function builder)
          (builder jit-builder rand-values)]
         [else (error "cannot figure out how to apply: ~a" rator)])]))
  (define entry-block (new-block 'entry))
  (LLVMPositionBuilderAtEnd jit-builder entry-block)
  (define new-env
    (for/fold ([env env])
              ([arg args]
               [type types]
               [i (in-range (length args))])
      (define l-type (build-env-type type env))
      (define p-arg (LLVMBuildAlloca jit-builder (internal-type-jit (env-type-prim l-type))
                                     (symbol->string arg)))
      (LLVMBuildStore jit-builder (LLVMGetParam fref i) p-arg)
      (env-extend arg (env-jit-value p-arg l-type) env)))

  (build-statement body new-env)
  env-function)

(define (compile-function-declaration env-function-type function-name jit-module)
  (define function-type (internal-type-jit (env-type-prim env-function-type)))
  (LLVMAddFunction jit-module (symbol->string function-name) function-type))

(define (compile-module m [module-name "module"] [context global-jit-context])
  (define (diag-handler dinfo voidp)
    (printf "\tllvm-diag: ~a\n" (LLVMGetDiagInfoDescription dinfo)))
  (LLVMContextSetDiagnosticHandler context diag-handler #f)
  (define jit-module (LLVMModuleCreateWithNameInContext module-name context))
  (LLVMSetDataLayout jit-module "e-m:e-i64:64-f80:128-n8:16:32:64-S128")
  (LLVMSetTarget jit-module "x86_64-unknown-linux-gnu") ;TODO set target for machine
  (define jit-builder (LLVMCreateBuilderInContext context))

  (define (register-module-statement stmt env)
    (match stmt
      [(sham:def:type type-name t)
       (env-extend type-name (build-env-type t env) env)]
      [(sham:def:function function-name _ _ args types ret-type body)
       (define type (build-env-type (sham:type:function types ret-type) env))
       (define function-obj (compile-function-declaration type function-name jit-module))
       (env-extend function-name
        	   (env-jit-function function-obj type)
        	   env)]))

  (define (compile-module-statement stmt env module-env)
    ;; TODO return values of name and obj
    (match stmt
      [(sham:def:type type-name t)
       ;types are created in register phase; something for recursive struct types to be done
       ;; TODO for struct switch to creating empty struct type and then adding fields here
       (env-extend type-name (env-lookup type-name env) module-env)]
      [(sham:def:function function-name passes attrs args types ret-type body)
       (printf "compiling-function ~a\n" function-name)
       (define f (compile-function-definition function-name args types ret-type
        				      body (env-lookup function-name env)
        				      env jit-module jit-builder))

       (for ([attr attrs])
         (LLVMAddAttributeAtIndex
          (env-jit-function-ref f)
          (cast -1 _int _uint)
          (jit-lookup-attr attr context)))
       (jit-run-function-pass passes jit-module f)
       (env-extend function-name f module-env)]))
  (match m
    [(sham:module passes defs)
     (define env
       (for/fold ([env (create-initial-environment context)])
                 ([stmt defs])
         (register-module-statement stmt env)))
     (define module-env
       (for/fold ([module-env (empty-env)])
                ([stmt defs])
         (compile-module-statement stmt env module-env)))
     (jit-run-module-pass passes jit-module)
     (LLVMVerifyModule jit-module 'LLVMPrintMessageAction #f)
     (env-extend '#%jit-module jit-module module-env)]))


(module+ test
  (require racket/unsafe/ops)
  (require rackunit)
  ;; (require "../disassemble/disassemble/main.rkt")

  (define i32 (sham:type:ref 'i32))
  (define module-env
    (compile-module
     (sham:module
       '(AlwaysInliner)
       (list
        (sham:def:type 'int i32)
        (sham:def:type 'int* (sham:type:pointer i32))
        
        (sham:def:type 'bc (sham:type:struct '(b c) (list i32 i32)))
        (sham:def:type 'pbc (sham:type:pointer (sham:type:ref 'bc)))
        (sham:def:type 'ui (sham:type:struct '(bc1 bc2) (list (sham:type:ref 'pbc)
                                                              (sham:type:ref 'pbc))))


        (sham:def:function 'const1 '() '() '() '() i32
                           (sham:stmt:return (sham:exp:ui-value 1 i32)))
        (sham:def:function 'id '() '() '(x) (list i32) i32
                           (sham:stmt:return (sham:exp:var 'x)))
        )

       ;; (define-function (even? (x : int) : int)
       ;;   (if (#%app jit-icmp-eq (#%app jit-urem x (#%value 2 int)) (#%value 0 int))
       ;;       (return (#%value 1 int))
       ;;       (return (#%value 0 int))))

       ;; (define-function (meven? (x : int) : int)
       ;;   (if (#%app jit-icmp-eq x (#%value 0 int))
       ;;       (return (#%value 1 int))
       ;;       (return (#%app modd? (#%app jit-sub x (#%value 1 int))))))

       ;; (define-function (modd? (x : int) : int)
       ;;   (if (#%app jit-icmp-eq x (#%value 0 int))
       ;;       (return (#%value 0 int))
       ;;       (return (#%app meven?
       ;;                      (#%app jit-sub x (#%value 1 int))))))

       ;; (define-function (fact (x : int) : int)
       ;;   (if (#%app jit-icmp-eq x (#%value 0 int))
       ;;       (return (#%value 1 int))
       ;;       (return (#%app jit-mul
       ;;                      x
       ;;                      (#%app fact
       ;;                             (#%app jit-sub x (#%value 1 int)))))))

       ;; (define-function (factc (x : int) : int)
       ;;   (let ((result* : int* (#%app jit-alloca (#%type int)))
       ;;         (i* : int* (#%app jit-alloca (#%type int)))
       ;;         (x* : int* (#%app jit-alloca (#%type int))))
       ;;     (block
       ;;      (#%exp (#%app jit-store! x x*))
       ;;      (#%exp (#%app jit-store! (#%value 1 int) result*))
       ;;      (#%exp (#%app jit-store! (#%value 1 int) i*))
       ;;      (while () (#%app jit-icmp-ule
       ;;                       (#%app jit-load i*)
       ;;                       (#%app jit-load x*))
       ;;             (block
       ;;              (#%exp (#%app jit-store! 
       ;;                            (#%app jit-mul-nuw (#%app jit-load result*)
       ;;                                   (#%app jit-load i*))
       ;;                            result*))
       ;;              (#%exp (#%app jit-store!
       ;;                      (#%app jit-add-nuw (#%app jit-load i*)
       ;;                             (#%value 1 int))
       ;;                      i*))))
       ;;      (return (#%app jit-load result*)))))

       ;; (define-function (factr (x : int) : int)
       ;;   (let ((result : int (#%value 1 int)))
       ;;     (let ((i : int (#%value 1 int)))
       ;;       (block
       ;;        (while ((result : int) (i : int))
       ;;          (#%app jit-icmp-ule i x)
       ;;          (block
       ;;           (set! result (#%app jit-mul-nuw result i))
       ;;           (set! i (#%app jit-add-nuw i (#%value 1 int)))))
       ;;        (return result)))))

       ;; (define-function (malloc-test  : int)
       ;;   (let ((ptr : void* (#%app jit-malloc (#%type int))))
       ;;     (block
       ;;      (#%exp (#%app jit-store! (#%value 42 int) ptr ))
       ;;      (return (#%app jit-load ptr)))))

       ;; (define-function (#:attr AlwaysInline) (add (x : int) (y : int) : int)
       ;;   (return (#%app jit-add x y)))

       ;; (define-function (sum-array (arr : int*) (size : int) : int)
       ;;   (let ((i : int (#%value 0 int))
       ;;         (sum : int (#%value 0 int)))
       ;;     (block
       ;;      (while ((sum : int) (i : int))
       ;;        (#%app jit-icmp-ult i size)
       ;;        (block
       ;;         (let ((arri : int* (#%gep arr (i))))
       ;;           (set! sum (#%app add
       ;;                            sum
       ;;                            (#%app jit-load arri))))
       ;;         (set! i (#%app jit-add i (#%value 1 int)))))
       ;;      (return sum))))
       )))

  (jit-dump-module module-env)
  (jit-optimize-module module-env #:opt-level 3)
  (jit-dump-module module-env)
  (jit-verify-module module-env)
  ;; (define cenv (initialize-jit module-env #:opt-level 3))

  ;; (define add (jit-get-function 'add cenv))

  ;; (define factr (jit-get-function 'factr cenv))
  ;; (define factc (jit-get-function 'factc cenv))
  ;; (define fact (jit-get-function 'fact cenv))
  ;; (define even? (jit-get-function 'even? cenv))
  ;; (define meven? (jit-get-function 'meven? cenv))
  ;; ;; (disassemble-ffi-function (jit-get-function-ptr 'sum-array cenv)
  ;; ;;                           #:size 70)

  ;; (check-eq? (add 3 5) 8)
  ;; (check-eq? (fact 5) 120)
  ;; (check-eq? (factr 5) 120)
  ;; (check-eq? (factc 5) 120)
  ;; (check-eq? (even? 42) 1)
  ;; (check-eq? (meven? 21) 0)

  ;; (define malloc-test (jit-get-function 'malloc-test cenv))
  ;; (check-eq? (malloc-test) 42)

  ;; (define sum-array (jit-get-function 'sum-array cenv))
  ;; (check-eq? (sum-array (list->cblock '(1 2 3) _uint) 3) 6)
  )
