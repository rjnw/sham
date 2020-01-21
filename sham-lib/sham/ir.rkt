#lang racket

(require sham/ast)
(require "ir/builder.rkt"
         "ir/dump.rkt"
         "ir/init.rkt"
         "ir/optimize.rkt"
         "ir/verify.rkt"
         "parameters.rkt"
         "higher.rkt")

(provide (all-from-out "ir/builder.rkt"
                       "ir/dump.rkt"
                       "ir/init.rkt"
                       "ir/optimize.rkt"
                       "ir/verify.rkt")
         ir-built?
         try-build-sham-ir!
         build-sham-ir!)


(define (ir-built? hm)
  (match-define (hmodule id func-map info hinfo imod) hm)
  (not (false? imod)))

(define (build-sham-ir! hm)
  (when (ir-built? hm) (error 'sham "internal llvm module already built" id))
  (match-define (hmodule id func-map info hinfo imod) hm)
  (define pre-special-funcs (hash-values func-map))
  (define all-defs (get-functions pre-special-funcs))
  (define special-defs (filter (λ (f) (not (member f pre-special-funcs))) (hash-values func-map)))
  (when (member 'pretty (build-options)) (for ([f all-defs]) (pretty-print f)))
  (define dm (dmodule info id (append all-defs (get-functions special-defs))))
  (define im (build-llvm-module dm id))
  (when (member 'verify (build-options)) (verify-llvm-module im))
  (when (member 'dump (build-options)) (dump-llvm-module im))
  (set-hmodule-internal! hm im))

(define (try-build-sham-ir! hm)
  (unless (ir-built? hm) (build-sham-ir! hm)))
