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


(define ptr-i32 (LLVMPointerType (LLVMInt32Type) 0))
(define param-types (list ptr-i32 (LLVMInt32Type)))
(define ret-type (LLVMFunctionType (LLVMInt32Type) param-types 0))
(define indx (LLVMAddFunction module "indx" ret-type))

(define entry-block (LLVMAppendBasicBlock indx "entry"))

(define builder (LLVMCreateBuilder))

(LLVMPositionBuilderAtEnd builder entry-block)
(define tmpptr (LLVMBuildGEP builder
                          (LLVMGetParam indx 0)
                          (list(LLVMGetParam indx 1))
                          "tmpptr"))
(define tmp (LLVMBuildLoad builder tmpptr "tmp"))
(define ret (LLVMBuildRet builder tmp))

(define verif (LLVMVerifyModule module 'LLVMPrintMessageAction #f))



(define args (list (LLVMCreateGenericValueOfPointer (list->cblock '(1 2 3 4 5) _int32))
                  (LLVMCreateGenericValueOfInt (LLVMInt32Type) 3 #f)))

(define-values (engine status err)
  (LLVMCreateMCJITCompilerForModule module (LLVMInitializeMCJITCompilerOptions)))

(define fptr (LLVMGetFunctionAddress engine "indx"))
(define fun (cast fptr _uint64 _pointer))
(define f (cast (cast fptr _uint64 _pointer) _pointer (_fun _pointer _int32 -> _int32)))
(printf "result: ~a\n" (f (list->cblock '( 1 2 3) _int32) 1))


(LLVMDisposeBuilder builder)
(LLVMDisposeModule module)
