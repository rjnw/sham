#lang racket
(require "define.rkt"
         "ctypes.rkt"
         ffi/unsafe)

(provide (all-defined-out))

(define-llvm LLVMLinkModules2 (_fun LLVMModuleRef LLVMModuleRef -> LLVMBool))
