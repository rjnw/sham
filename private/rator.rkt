#lang racket

(require ffi/unsafe (prefix-in foreign: '#%foreign))
(require "info-key.rkt"
         "llvm/ffi/all.rkt")

(provide (all-defined-out))

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

(define ffi-mapping-key 'ffi-mappings)
(define ffi-lib-key 'ffi-libs)
(define rkt-mapping-key 'rkt-mappings)

(define (add-ffi-mappings engine mod-env)
  (define ffi-mappings (jit-get-info-key ffi-mapping-key mod-env))
  (define ffi-libs (jit-get-info-key ffi-lib-key mod-env))
  (unless (or (empty? ffi-mappings) (void? ffi-libs))
    (define lib-map (for/hash ([fl ffi-libs])
                      (match fl
                        [`(,lib-name . (,str-args ... #:global? ,gv))
                         (define flib (apply ffi-lib str-args #:global? gv))
                         (values lib-name flib)]
                        [`(,lib-name . (,str-args ...))
                         (define flib (apply ffi-lib str-args))
                         (values lib-name flib)])))
    (for [(ffi-mapping ffi-mappings)]
      (match ffi-mapping
        [`(,name ,lib-name ,value)
         (define fflib (hash-ref lib-map lib-name))
         (LLVMAddGlobalMapping engine value
                               (get-ffi-pointer fflib (symbol->string name)))]))))

(define (add-rkt-mappings engine mod-env)
  (define rkt-mappings (jit-get-info-key rkt-mapping-key mod-env))
  (for ([(id rkt-mapping) (in-hash rkt-mappings)])
    (match-define (list rkt-fun rkt-type jit-value)  rkt-mapping)
    (define rfc (cast rkt-fun _scheme rkt-type))
    (define rfs (cast rfc _scheme _ffi_obj_struct-pointer))
    (LLVMAddGlobalMapping engine jit-value (cast (function-ptr rkt-fun rkt-type) _pointer _uint64))))


(module+ test
  (define libc (ffi-lib "libc" "6"))
  (get-ffi-pointer libc "gets")
  (define (id x) x)
  (define idc (cast id _scheme (_fun _uint -> _uint)))
  (define idcf (cast idc _scheme _ffi_obj_struct-pointer))
  (printf "name: ~a\n" (ffi_obj_struct-name idcf)))
