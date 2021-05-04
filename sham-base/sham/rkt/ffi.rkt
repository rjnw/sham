#lang racket

(require ffi/unsafe
         (prefix-in foreign: '#%foreign))

(provide get-lib-uintptr)

(define-cstruct _scheme_object  ([typetag _short]   [key _short]))
(define-cstruct _ffi_obj_struct
  ([iso _scheme_object]
   [obj _uintptr]
   [name _string]
   [flib _pointer]))

(define (get-lib-uintptr ffi-lib fname)
  (define fptr-obj (foreign:ffi-obj (string->bytes/locale fname) ffi-lib))
  (define fname-ffi-obj (cast fptr-obj _scheme _ffi_obj_struct-pointer))
  (ffi_obj_struct-obj fname-ffi-obj))
