#lang racket

(require ffi/unsafe)
(require sham/llvm
         sham/ast
         sham/ir
         sham/jit)
(require "higher.rkt"
         "parameters.rkt")

(provide (all-defined-out))
(define (rptr->llvmptr rptr)
  (cllvm
   (LLVMConstIntToPtr (LLVMConstInt (LLVMInt64Type) (cast rptr _pointer _uintptr) #f)
                      (LLVMPointerType (LLVMInt8Type) 0))
   i8*))

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

(define (optimize-sham-module! hm
                               #:opt-level (opt-level 1)
                               #:size-level (size-level 1)
                               #:loop-vec (loop-vec #f))
  (try-build-sham-ir! hm)
  (match-define (hmodule id func-map info hinfo lmod) hm)
  (optimize-llvm-module lmod #:opt-level opt-level #:size-level size-level #:loop-vec loop-vec))

(define (compile-sham-module! hm
                              #:opt-level (opt-level 1)
                              #:size-level (size-level 1)
                              #:loop-vec (loop-vec #f))
  (optimize-sham-module! hm #:opt-level opt-level #:size-level size-level #:loop-vec loop-vec)
  (match-define (hmodule id func-map info hinfo cm) hm)
  (when (member 'dump (compile-options)) (dump-llvm-module cm))
  (cond
    [(member 'mc-jit (compile-options)) (module-initialize-mcjit! cm)]
    [(member 'orc (compile-options)) (module-initialize-orc! cm)]
    [else (module-initialize-mcjit! cm)]))

(define (merge-sham-module into from)
  (define (combine-info inf1 inf2)
    ;; (pretty-print inf1) (pretty-print inf2)
    ;; (printf "TODO: combine infos")
    inf2)
  (define (combine-hinfo inf1 inf2) inf1) ;TODO
  (match-define (hmodule into-id into-func-map into-info into-hinfo into-cmod) into)
  (match-define (hmodule from-id from-func-map from-info from-hinfo from-cmod) from)
  (when (or into-cmod from-cmod)
    (error "one or both sham modules already compiled before merging" into-id from-id))

  (define new-func-map (make-hash (append (hash->list into-func-map) (hash->list from-func-map))))
  (hmodule into-id
           new-func-map
           (combine-info into-info from-info)
           (combine-hinfo into-hinfo into-hinfo)
           #f))

(define (sham-module-lookup-function hm fid)
  (if (hmodule-internal hm)
      (jit-get-function fid (hmodule-internal hm))
      (error "module not compiled.")))

(define (sham-app hfunc . args)
  (match-define (hfunction id sargs argt rett bb finfo sm) hfunc)
  (apply (sham-module-lookup-function sm id) args))
(define (get-sham-function hfunc)
  (match-define (hfunction id sargs argt rett bb finfo sm) hfunc)
  (sham-module-lookup-function sm id))

(define (sham-dump-llvm hm)
  (match-define (hmodule id func-map info hinfo cmod) hm)
  (dump-llvm-module cmod))
