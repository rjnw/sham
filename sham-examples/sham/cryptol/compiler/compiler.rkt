#lang racket

(provide (all-defined-out))

(struct compiler
  [
   def-val
   function-arg
   function-result

   type
   type-sequence
   type-int
   type-tuple
   ])

(define (compile-tuple-index val i) #`(ll-op-gep #,val 0 #,i))
(define (compile-sequence-index val i) #`(ll-op-gep #,val 1 #,i))
(define (compile-function-arg arg-type arg-index ftype ctxt) #f)
(define (compile-function-result res-type ftype ctxt) #f)
(define (compile-def-val name body ctxt) #f)

(define test-compiler (compiler))

(define current-compiler (make-parameter test-compiler))
