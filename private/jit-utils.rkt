#lang racket


(require
 "llvm/ffi/all.rkt"
 "jit.rkt"
 "ast-utils.rkt"
 "optimize.rkt"
 "init.rkt"
 "parameters.rkt"
 ffi/unsafe
 )


(define (rptr->llvmptr rptr)
  (cllvm
   (LLVMConstIntToPtr (LLVMConstInt (LLVMInt64Type) (cast rptr _pointer _uintptr) #f)
                      (LLVMPointerType (LLVMInt8Type) 0))
   i8*))


(define (compile-sham-module! hm #:opt-level (opt-level #f) #:size-level (size-level 1))
  (match-define (hmodule id func-map info cmod) hm)
  (when cmod
    (error "sham-module already compiled" id))
  (define dm (dmodule info id (get-functions (hash-values func-map))))
  (define cm (jit-module-compile dm id))
  (when opt-level (jit-optimize-module cm #:optlevel opt-level #:size-level size-level))
  (module-initialize-orc! cm)
  (set-hmodule-cmod! hm cm))
(define (get-module-func hm fid)
  (jit-get-function fid (hmodule-cmod hm)))

(define (sham-app hfunc . args)
  (match-define (hfunction id sargs argt rett bb sm) hfunc)
  (apply (get-module-func sm id) args))
(define (sham-function hfunc)
  (match-define (hfunction id sargs argt rett bb sm) hfunc)
  (get-module-func sm id))
