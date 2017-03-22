#lang racket
(require "../llvm/ffi/all.rkt")
(require ffi/unsafe)

(LLVMLinkInMCJIT)
(LLVMInitializeX86Target)
(LLVMInitializeX86TargetInfo)
(LLVMInitializeX86TargetMC)
(LLVMInitializeX86AsmParser)
(LLVMInitializeX86AsmPrinter)

(define module (LLVMModuleCreateWithName "test_module"))
(define builder (LLVMCreateBuilder))

(define int64 (LLVMInt64Type))
(define int32 (LLVMInt32Type))

(define int64* (LLVMPointerType int64 0))

(define param-types (list int64* int64* int64))
(define fun-type (LLVMFunctionType int64 param-types #f))
(printf "hello")
(define dotp (LLVMAddFunction module "dotp" fun-type))

(define entry-block (LLVMAppendBasicBlock dotp "entry"))
(define loop-block (LLVMAppendBasicBlock dotp "loop"))
(define afterloop-block (LLVMAppendBasicBlock dotp "afterloop"))

(define arr1 (LLVMGetParam dotp 0))
(define arr2 (LLVMGetParam dotp 1))
(define arg3 (LLVMGetParam dotp 2))

(define zero (LLVMConstInt int32 0 #f))
(define one (LLVMConstInt int64 1 #f))
(define five (LLVMConstInt int64 20 #f))

(LLVMPositionBuilderAtEnd builder entry-block)
(LLVMBuildBr builder loop-block)

(LLVMPositionBuilderAtEnd builder loop-block)
(printf "entering loopblock")
(define i (LLVMBuildPhi builder int64 "i"))
(define prevsum (LLVMBuildPhi builder int64 "prevsum"))

(define lc (LLVMBuildICmp builder 'LLVMIntEQ i zero "loopcond"))
(define arr1vp (LLVMBuildGEP builder
                            arr1
                            (list i)
                            "arr1vp"))
(define arr1v (LLVMBuildLoad builder arr1vp "arr1v"))
(define arr2vp (LLVMBuildGEP builder
                            arr2
                            (list i)
                            "arr2v"))
(define arr2v (LLVMBuildLoad builder arr2vp "arr2v"))
(define nextsum (LLVMBuildAdd builder
                              arr1v
                              (LLVMBuildAdd builder prevsum arr2v "nsump1")
                              "nsump2"))
(printf "built sum")
(define nextval (LLVMBuildSub builder i one "nextval"))
(LLVMBuildCondBr builder lc afterloop-block loop-block)

(LLVMAddIncoming i (list arg3 nextval) (list entry-block loop-block))
(LLVMAddIncoming prevsum (list zero nextsum) (list entry-block loop-block))

(LLVMPositionBuilderAtEnd builder afterloop-block)
(define totalsum (LLVMBuildPhi builder int64 "totalsum"))
(LLVMAddIncoming totalsum (list nextsum) (list loop-block))
(LLVMBuildRet builder totalsum)

(LLVMDumpModule module)
(define verif (LLVMVerifyModule module 'LLVMPrintMessageAction #f))


;; (define-values (engine status err) (LLVMCreateExecutionEngineForModule module))
;; (printf "engine err: ~a\n" err)


;; (define args (list (LLVMCreateGenericValueOfPointer (list->cblock biglist _int64))
;;                    (LLVMCreateGenericValueOfPointer (list->cblock biglist _int64))
;;                    (LLVMCreateGenericValueOfInt (LLVMInt64Type) (length biglist) #f)))

;; (define res (LLVMRunFunction engine dotp args))
;; (printf "result: ~a\n" (LLVMGenericValueToInt res #f))

(define fpm (LLVMCreateFunctionPassManagerForModule module))
(LLVMAddIndVarSimplifyPass fpm)
(LLVMAddLoopUnrollPass fpm)
(LLVMAddLoopVectorizePass fpm)
(LLVMAddBBVectorizePass fpm)
(LLVMAddSLPVectorizePass fpm)

(define change (LLVMRunFunctionPassManager fpm dotp))
(printf "change: ~a" change)
(LLVMDumpModule module)

(define fpmwithb (LLVMCreateFunctionPassManagerForModule module))
(define fbpm (LLVMPassManagerBuilderCreate))
(LLVMPassManagerBuilderSetOptLevel fbpm 3)
(LLVMPassManagerBuilderSetSizeLevel fbpm 1000)
(LLVMPassManagerBuilderPopulateFunctionPassManager fbpm fpmwithb)

(define change2 (LLVMRunFunctionPassManager fpmwithb dotp))
(printf "change2: ~a\n" change2)
(LLVMDumpModule module)

(define mcjit-options (LLVMInitializeMCJITCompilerOptions))
(set-LLVMMCJITCompilerOptions-OptLevel! mcjit-options 2)
(define-values (engine status err)
  (LLVMCreateMCJITCompilerForModule module mcjit-options))
(printf "mcjit optlevel ~a\n" (LLVMMCJITCompilerOptions-OptLevel mcjit-options))

(define fptr (LLVMGetFunctionAddress engine "dotp"))

(define fun (cast fptr _uint64 _pointer))
;; (define fbytes (cast fun _pointer _bytes))
(define f (cast (cast fptr _uint64 _pointer) _pointer (_fun _pointer _pointer _int64 -> _int64)))

(define biglist (stream->list  (in-range 10000)))
  
(define bigvec (for/vector ([i (in-range 10000)])
                 i))

(define bg (list->cblock biglist _int64))

(printf "result: ~a\n" (time (begin
                               (f bg bg  (length biglist))
                               (f bg bg  (length biglist))
                               (f bg bg  (length biglist))
                               (f bg bg  (length biglist)))))

(time
   (begin
     (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
       (+ a1 a2))
     (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
       (+ a1 a2))
     (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
       (+ a1 a2))
     (for/sum [(a1 (in-vector bigvec))
                   (a2 (in-vector bigvec))]
           (+ a1 a2))))

(LLVMPassManagerBuilderDispose fbpm)

;; (require "../../disassemble/disassemble/main.rkt")
;; (disassemble-ffi-function fun #:size 100)
