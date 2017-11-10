#lang racket

(require ffi/unsafe (prefix-in foreign: '#%foreign))

(provide get-ffi-pointer)
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

(module+ test
  (define libc (ffi-lib "libc" "6"))
  (get-ffi-pointer libc "gets")
  (define (id x) x)
  (define idc (cast id _scheme (_fun _uint -> _uint)))
  (define idcf (cast idc _scheme _ffi_obj_struct-pointer))
  (printf "name: ~a\n" (ffi_obj_struct-name idcf)))
