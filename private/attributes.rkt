#lang racket

(require "llvm/pass-table.rkt"
         "llvm/ffi/all.rkt"
         "info-key.rkt")

(provide add-function-attributes)

(define function-attribute-index (modulo -1 (expt 2 32)))

;;function attributes are a list of symbols
(define (add-function-attributes lf jit-module context info)
  (do-if-info-key
   function-attributes-info-key info attrs
   (for ([attr attrs])
     (LLVMAddAttributeAtIndex
      lf function-attribute-index (lookup-attribute attr context)))))
