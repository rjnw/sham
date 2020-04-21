#lang racket

(require sham/llvm/ffi
         sham/llvm/ir/env
         ffi/unsafe)

(provide (all-defined-out))

(define calling-convention-key 'calling-convention)
(define (get-calling-convention md-info)
  (cond
    [(assoc-env? md-info) (assoc-env-ref md-info calling-convention-key)]
    [(hash? md-info) (hash-ref md-info calling-convention-key #f)]
    [else #f]))

(define (add-instruction-md! value md flags)
  (define conv (or (get-calling-convention md)
                   (get-calling-convention flags)))
  (when conv (set-instruction-call-conv! value conv))
  value)
(define (add-type-def-info! value info) value)
(define (add-function-def-info! value info)
  (define conv (get-calling-convention info))
  (when conv (set-function-call-conv! value conv))
  value)

;; calling convention
(define (llvm-calling-convention->uint call-conv)
  (cast (string->symbol (format "LLVM~aCallConv" call-conv))
        LLVMCallConv _uint))
(define (uint->llvm-calling-convention i)
  (cast i _uint LLVMCallConv))

(define (set-instruction-call-conv! cs conv)
  (LLVMSetInstructionCallConv cs (llvm-calling-convention->uint conv)))
(define (get-instruction-call-conv cs)
  (uint->llvm-calling-convention (LLVMGetInstructionCallConv cs)))

(define (set-function-call-conv! f conv)
  (LLVMSetFunctionCallConv f (llvm-calling-convention->uint conv)))
(define (get-function-call-conv f)
  (uint->llvm-calling-convention (LLVMGetFunctionCallConv f)))

;; module info
(define (empty-module-info) (make-hash))

(define module-info-external-mapping-key 'llvm-external-mappings)

(define (info-lambda key)
  (case-lambda
    [(info) (cond [(hash? info) (hash-ref info key #f)]
                  [(assoc-env? info) (assoc-env-ref info key)]
                  [else #f])]
    [(info value) (cond
                    [(and (immutable? info) (hash? info)) (hash-set info key value)]
                    [(hash? info) (begin (hash-set! info key value) info)]
                    [(assoc-env? info) (assoc-env-extend info key value)]
                    [(false? info) (hash-set! (empty-module-info) key value)]
                    [else (error 'sham:llvm "unknown module info value:~a" info)])]))

(define module-info-external-mappings (info-lambda module-info-external-mapping-key))

;; function info
(define (empty-function-info) (make-hash))
