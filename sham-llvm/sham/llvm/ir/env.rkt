#lang racket

(provide (all-defined-out))

(struct llvm-value [ref type] #:prefab)
(struct llvm-function llvm-value [] #:prefab)
(struct llvm-external llvm-value [] #:prefab)
(struct llvm-intrinsic llvm-value [] #:prefab)

(struct llvm-env [module-ref context-ref ast value-refs])

(define (llvm-env-lookup-value env key)
  (hash-ref (llvm-env-value-refs env) key))
(define (llvm-env-values env)
  (hash-values (llvm-env-value-refs env)))

(define (llvm-env-contains-value? env key)
  (hash-has-key? (llvm-env-value-refs env) key))

(define (llvm-env-add-value! env key value)
  (hash-set! (llvm-env-value-refs env) key value))

;; external mapping

(struct external-mapping [name uintptr] #:prefab)
