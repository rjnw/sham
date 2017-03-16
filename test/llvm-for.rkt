#lang racket
(require "../llvm/ffi/all.rkt")
(require ffi/unsafe)

(define module (LLVMModuleCreateWithName "test_module"))
(define builder (LLVMCreateBuilder))

(define int32 (LLVMInt32Type))
(define param-types (list (LLVMInt32Type)))
(define ret-type (LLVMFunctionType (LLVMInt32Type) param-types #f))

(define summ (LLVMAddFunction module "summ" ret-type))

(define entry-block (LLVMAppendBasicBlock summ "entry"))
(define loop-block (LLVMAppendBasicBlock summ "loop"))
(define afterloop-block (LLVMAppendBasicBlock summ "afterloop"))

(define arg1 (LLVMGetParam summ 0))
(define zero (LLVMConstInt int32 0 #f))
(define one (LLVMConstInt int32 1 #f))

(LLVMPositionBuilderAtEnd builder entry-block)
(LLVMBuildBr builder loop-block)

(LLVMPositionBuilderAtEnd builder loop-block)

(define i (LLVMBuildPhi builder int32 "i"))
(define prevsum (LLVMBuildPhi builder int32 "prevsum"))

(define lc (LLVMBuildICmp builder 'LLVMIntEQ i zero "loopcond"))
(define nextsum (LLVMBuildAdd builder prevsum one "nextsum"))
(define nextval (LLVMBuildSub builder i one "nextval"))
(LLVMBuildCondBr builder lc afterloop-block loop-block)

(LLVMAddIncoming i (list arg1 nextval) (list entry-block loop-block))
(LLVMAddIncoming prevsum (list zero nextsum) (list entry-block loop-block))

(LLVMPositionBuilderAtEnd builder afterloop-block)
(define totalsum (LLVMBuildPhi builder int32 "totalsum"))
(LLVMAddIncoming totalsum (list nextsum) (list loop-block))
(LLVMBuildRet builder totalsum)

(LLVMDumpModule module)
(define verif (LLVMVerifyModule module 'LLVMPrintMessageAction #f))


(define-values (engine status err) (LLVMCreateExecutionEngineForModule module))


(define args (list (LLVMCreateGenericValueOfInt (LLVMInt32Type) 5 #f)))

(define res (LLVMRunFunction engine summ args))
(printf "result: ~a\n" (LLVMGenericValueToInt res #f))


(define fpm (LLVMCreateFunctionPassManagerForModule module))
(define fbpm (LLVMPassManagerBuilderCreate))
(LLVMPassManagerBuilderSetOptLevel fbpm 3)
(LLVMPassManagerBuilderPopulateFunctionPassManager fbpm fpm)
(LLVMPassManagerBuilderDispose fbpm)
(define change (LLVMRunFunctionPassManager fpm summ))
(printf "change: ~a" change)
(LLVMDumpModule module)
