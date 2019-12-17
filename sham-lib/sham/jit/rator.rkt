#lang racket

(require ffi/unsafe (prefix-in foreign: '#%foreign))

(require sham/llvm/ffi/all
         "module-env.rkt")

(provide add-ffi-mappings add-rkt-mappings)

(define-cstruct _scheme_object  ([typetag _short]   [key _short]))
(define-cstruct _ffi_obj_struct
  ([iso _scheme_object]
   [obj _uintptr]
   [name _string]
   [flib _pointer]))

(define (get-ffi-pointer ffi-lib fname)
  (define fptr-obj (foreign:ffi-obj (string->bytes/locale fname) ffi-lib))
  (define fname-ffi-obj (cast fptr-obj _scheme _ffi_obj_struct-pointer))
  (ffi_obj_struct-obj fname-ffi-obj))

(define (load-ffi-libs mod-env)
  (for/hash ([fl (env-get-info-key mod-env ffi-lib-key '())])
    (match fl
      [`(,lib-name . (,str-args ... #:global? ,gv))
       (define flib (apply ffi-lib str-args #:global? gv))
       (values lib-name flib)]
      [`(,lib-name . (,str-args ...))
       (define flib (apply ffi-lib str-args))
       (values lib-name flib)])))

(define (add-ffi-mappings mod-env)
  (define mcjit (env-get-mcjit mod-env))
  (define lib-map (load-ffi-libs mod-env))
  (do-if-info-key
   ffi-mapping-key (env-get-info mod-env) ffi-mappings
   (for ([(name lib-v) (in-hash ffi-mappings)])
     (match-define (cons lib-name value) lib-v)
     (define fptr (get-ffi-pointer (hash-ref lib-map lib-name #f) (symbol->string name)))
     (LLVMAddGlobalMapping mcjit value fptr))))

(define (add-rkt-mappings mod-env)
  (define mcjit (env-get-mcjit mod-env))
  (do-if-info-key
   rkt-mapping-key (env-get-info mod-env) rkt-mappings
   (for ([(id v) (in-hash rkt-mappings)])
     (match-define (list rkt-fun rkt-type jit-value) v)
     (LLVMAddGlobalMapping mcjit jit-value (cast (function-ptr rkt-fun rkt-type) _pointer _uint64)))))

(module+ test
  (define libc (ffi-lib "libc" "6"))
  (get-ffi-pointer libc "gets")
  (define (id x) x)
  (define idc (cast id _scheme (_fun _uint -> _uint)))
  (define idcf (cast idc _scheme _ffi_obj_struct-pointer))
  (printf "name: ~a\n" (ffi_obj_struct-name idcf)))
