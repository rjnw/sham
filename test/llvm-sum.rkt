#lang racket
(require "../llvm/ffi/all.rkt")
(require ffi/unsafe)

(define module (LLVMModuleCreateWithName "test_module"))
(define param-types (list (LLVMInt32Type) (LLVMInt32Type)))
(define ret-type (LLVMFunctionType (LLVMInt32Type) param-types 0))
(define sum-f (LLVMAddFunction module "sum" ret-type))

(define entry-block (LLVMAppendBasicBlock sum-f "entry"))

(define builder (LLVMCreateBuilder))

(LLVMPositionBuilderAtEnd builder entry-block)

(define tmp (LLVMBuildAdd builder
                          (LLVMGetParam sum-f 0)
                          (LLVMGetParam sum-f 1)
                          "tmp"))
(define ret (LLVMBuildRet builder tmp))

(define verif (LLVMVerifyModule module 'LLVMPrintMessageAction #f))


(define-values (engine status err) (LLVMCreateExecutionEngineForModule module))


(define args (list (LLVMCreateGenericValueOfInt (LLVMInt32Type) 5 #f)
                   (LLVMCreateGenericValueOfInt (LLVMInt32Type) 4 #f)))

(define res (LLVMRunFunction engine sum-f args))
(printf "result: ~a\n" (LLVMGenericValueToInt res #f))

(LLVMWriteBitcodeToFile module "test.bc")
