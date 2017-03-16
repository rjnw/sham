#lang racket
(require "../llvm/ffi/all.rkt")
(require ffi/unsafe)

(define module (LLVMModuleCreateWithName "test_module"))

(define int32 (LLVMInt32Type))
(define param-types (list (LLVMInt32Type) (LLVMInt32Type) (LLVMInt32Type)))
(define ret-type (LLVMFunctionType (LLVMInt32Type) param-types #f))
(define iff (LLVMAddFunction module "iffun" ret-type))

(define entry-block (LLVMAppendBasicBlock iff "entry"))
(define then-block (LLVMAppendBasicBlock iff "then"))
(define else-block (LLVMAppendBasicBlock iff "else"))
(define ifcont-block (LLVMAppendBasicBlock iff "ifcont"))

(define builder (LLVMCreateBuilder))

(LLVMPositionBuilderAtEnd builder entry-block)

(define arg1 (LLVMGetParam iff 0))
(define arg2 (LLVMGetParam iff 1))
(define arg3 (LLVMGetParam iff 2))

(define zero (LLVMConstInt int32 0 #f))
(define ifcond (LLVMBuildICmp builder 'LLVMIntEQ arg1 zero "ifcond"))
(LLVMBuildCondBr builder ifcond then-block else-block)

(LLVMPositionBuilderAtEnd builder then-block)
(define thenv (LLVMBuildAdd builder arg2 zero "thenv"))
(LLVMBuildBr builder ifcont-block)

(LLVMPositionBuilderAtEnd builder else-block)
(define elsev (LLVMBuildAdd builder arg3 zero "elsev"))
(LLVMBuildBr builder ifcont-block)

(LLVMPositionBuilderAtEnd builder ifcont-block)
(define phi (LLVMBuildPhi builder int32 "ifv"))
(LLVMAddIncoming phi (list thenv elsev) (list then-block else-block))
(LLVMBuildRet builder phi)
(define verif (LLVMVerifyModule module 'LLVMPrintMessageAction #f))


(define-values (engine status err) (LLVMCreateExecutionEngineForModule module))


(define args (list (LLVMCreateGenericValueOfInt (LLVMInt32Type) 0 #f)
                   (LLVMCreateGenericValueOfInt (LLVMInt32Type) 4 #f)
                   (LLVMCreateGenericValueOfInt (LLVMInt32Type) 5 #f)))

(define res (LLVMRunFunction engine iff args))
(printf "result: ~a\n" (LLVMGenericValueToInt res #f))

(LLVMDumpModule module)
(define fpm (LLVMCreateFunctionPassManagerForModule module))
(define fbpm (LLVMPassManagerBuilderCreate))
(LLVMPassManagerBuilderSetOptLevel fbpm 1)
(LLVMPassManagerBuilderPopulateFunctionPassManager fbpm fpm)
(LLVMPassManagerBuilderDispose fbpm)
(define change (LLVMRunFunctionPassManager fpm iff))
(printf "change: ~a" change)
(LLVMDumpModule module)
