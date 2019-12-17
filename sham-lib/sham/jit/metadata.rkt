#lang racket
;(require "llvm/ffi/all.rkt")

;; REFERENCE for building metadata information

;; this adds a named metadata to the module
;; LLVMValueRef is the metadata node generated using LLVMMDNode
;; (define-llvm LLVMAddNamedMetadataOperand (_fun LLVMModuleRef _string LLVMValueRef -> _void))


;; (define-llvm LLVMMDStringInContext (_fun LLVMContextRef _string _uint -> LLVMValueRef))
;; (define-llvm LLVMMDString (_fun _string _uint -> LLVMValueRef))
;; (define-llvm LLVMMDNodeInContext (_fun LLVMContextRef (pointer-to LLVMValueRef) _uint -> LLVMValueRef))

;; ;; takes a list of values and creates a metadata
;; (define-llvm LLVMMDNode (_fun (vals : (_list i  LLVMValueRef)) (size : _uint = (length vals)) -> LLVMValueRef))
;; (define-llvm LLVMMetadataAsValue (_fun LLVMContextRef LLVMMetadataRef -> LLVMValueRef))
;; (define-llvm LLVMValueAsMetadata (_fun LLVMValueRef -> LLVMMetadataRef))

;; (define-llvm LLVMGetMDString (_fun LLVMValueRef (pointer-to unsigned) -> _string))
;; (define-llvm LLVMGetMDNodeNumOperands (_fun LLVMValueRef -> unsigned))
;; (define-llvm LLVMGetMDNodeOperands (_fun LLVMValueRef (pointer-to LLVMValueRef) -> _void))


;; (define-llvm LLVMHasMetadata (_fun LLVMValueRef -> _int))
;; (define-llvm LLVMGetMetadata (_fun LLVMValueRef _uint -> LLVMValueRef))
;; (define-llvm LLVMSetMetadata (_fun LLVMValueRef _uint LLVMValueRef -> _void))

;; (define-llvm LLVMGetMDKindIDInContext (_fun LLVMContextRef _string _uint -> unsigned))


;; int llvm_add_named_metadata_operand(void) {}
;;   LLVMModuleRef m = LLVMModuleCreateWithName("Mod");
;;   LLVMValueRef values[] = { LLVMConstInt(LLVMInt32Type(), 0, 0)};

;;   // This used to trigger an assertion
;;   LLVMAddNamedMetadataOperand(m, "name", LLVMMDNode(values, 1));

;;   LLVMDisposeModule(m);

;;   return 0;


;; int llvm_set_metadata(void) {}
;;   LLVMBuilderRef b = LLVMCreateBuilder();
;;   LLVMValueRef values[] = { LLVMConstInt(LLVMInt32Type(), 0, 0)};

;;   // This used to trigger an assertion
;;   LLVMSetMetadata()
;;       LLVMBuildRetVoid(b),
;;       LLVMGetMDKindID("kind", 4),
;;       LLVMMDNode(values, 1);

;;   LLVMDisposeBuilder(b);

;;   return 0;
