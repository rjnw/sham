#lang info

(define collection 'multi)

;; (define deps '("sham-llvm" "sham-ast" "sham-jit" "sham-doc"))
(define deps '("sham-llvm" "sham-lib"))
(define implies deps)

(define pkg-desc "A DSL for compiling low level code at runtime.")
