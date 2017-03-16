#lang racket
(require "../llvm/ffi/all.rkt")
(require ffi/unsafe)

(define module (LLVMModuleCreateWithName "test_module"))
(define builder (LLVMCreateBuilder))

(define int32 (LLVMInt32Type))
(define int32* (LLVMPointerType int32 0))

(define param-types (list int32* int32* int32))
(define fun-type (LLVMFunctionType int32 param-types #f))
(printf "hello")
(define dotp (LLVMAddFunction module "dotp" fun-type))

(define entry-block (LLVMAppendBasicBlock dotp "entry"))
(define loop-block (LLVMAppendBasicBlock dotp "loop"))
(define afterloop-block (LLVMAppendBasicBlock dotp "afterloop"))

(define arr1 (LLVMGetParam dotp 0))
(define arr2 (LLVMGetParam dotp 1))
(define arg3 (LLVMGetParam dotp 2))

(define zero (LLVMConstInt int32 0 #f))
(define one (LLVMConstInt int32 1 #f))
(define five (LLVMConstInt int32 20 #f))

(LLVMPositionBuilderAtEnd builder entry-block)
(LLVMBuildBr builder loop-block)

(LLVMPositionBuilderAtEnd builder loop-block)
(printf "entering loopblock")
(define i (LLVMBuildPhi builder int32 "i"))
(define prevsum (LLVMBuildPhi builder int32 "prevsum"))

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

(LLVMAddIncoming i (list five nextval) (list entry-block loop-block))
(LLVMAddIncoming prevsum (list zero nextsum) (list entry-block loop-block))

(LLVMPositionBuilderAtEnd builder afterloop-block)
(define totalsum (LLVMBuildPhi builder int32 "totalsum"))
(LLVMAddIncoming totalsum (list nextsum) (list loop-block))
(LLVMBuildRet builder totalsum)

(LLVMDumpModule module)
(define verif (LLVMVerifyModule module 'LLVMPrintMessageAction #f))


(define-values (engine status err) (LLVMCreateExecutionEngineForModule module))
(printf "engine err: ~a\n" err)

(define args (list (LLVMCreateGenericValueOfPointer (list->cblock '(1 2 3 4 5 6) _int32))
                   (LLVMCreateGenericValueOfPointer (list->cblock '(1 2 3 4 5 6) _int32))
                   (LLVMCreateGenericValueOfInt (LLVMInt32Type) 3 #f)))

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
(printf "change2: ~a" change2)
(LLVMDumpModule module)



(LLVMPassManagerBuilderDispose fbpm)
