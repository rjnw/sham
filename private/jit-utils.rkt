#lang racket


(require
 "llvm/ffi/all.rkt"
 "jit.rkt"
 "ast-utils.rkt"
 "optimize.rkt"
 "init.rkt"
 "parameters.rkt"
 "dump.rkt"
 ffi/unsafe
 )

(provide (all-defined-out))
(define (rptr->llvmptr rptr)
  (cllvm
   (LLVMConstIntToPtr (LLVMConstInt (LLVMInt64Type) (cast rptr _pointer _uintptr) #f)
                      (LLVMPointerType (LLVMInt8Type) 0))
   i8*))


(define (compile-sham-module! hm
                              #:opt-level (opt-level 1)
                              #:size-level (size-level 1)
                              #:loop-vec (loop-vec #f))
  (match-define (hmodule id func-map info cmod) hm)
  (when cmod
    (error "sham-module already compiled" id))
  (define all-defs (get-functions (hash-values func-map)))
  (define dm (dmodule info id all-defs))
  (when (member 'pretty (compile-options)) (for ([f all-defs]) (pretty-print f)))
  (define cm (jit-module-compile dm id))
  (when (member 'verify (compile-options)) (jit-verify-module cm))
  (when (member 'dump (compile-options)) (jit-dump-module cm))
  (jit-optimize-module cm #:opt-level opt-level #:size-level size-level #:loop-vec loop-vec)
  (when (member 'dump (compile-options)) (jit-dump-module cm))

  (cond
    [(member 'mc-jit (compile-options)) (module-initialize-mcjit! cm)]
    [else (module-initialize-orc! cm)])
  (set-hmodule-cmod! hm cm))

(define (get-module-func hm fid)
  (if (hmodule-cmod hm)
      (jit-get-function fid (hmodule-cmod hm))
      (error "module not compiled.")))

(define (sham-app hfunc . args)
  (match-define (hfunction id sargs argt rett bb finfo sm) hfunc)
  (apply (get-module-func sm id) args))
(define (sham-function hfunc)
  (match-define (hfunction id sargs argt rett bb finfo sm) hfunc)
  (get-module-func sm id))

(define (sham-dump-llvm hm)
  (match-define (hmodule id func-map info cmod) hm)
  (jit-dump-module cmod))
