#lang racket

(require sham/md
         sham/ir
         sham/llvm/ir/md
         sham/llvm/ir/env
         sham/jit
         sham/rkt
         ffi/unsafe
         (prefix-in ll- sham/llvm/ir))

(define ex-rkt
  (external-mapping
   'printf
   (cast (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _fpointer) _fpointer _uintptr)
   ;; (ptr-ref (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _fpointer) _uintptr)
   ;; (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _uintptr)
   ))

(define printf-def (ll-def-external 'printf (ll-type-function (ll-type-pointer i8) #t i64)))

(define print-foo
  (function (print-foo : i64) (stmt-return (expr-app 'printf (ll-val-string "foo: %d\n") (ui64 0)))))

(define sham-md
  (sham-module-metadata #:externals (list  ;; ex-rkt
                                     )))

(define sham-module (def-module #:md sham-md "test-module" printf-def print-foo))

(define built-sham-env (build-sham-env sham-module))
(sham-print-llvm-ir built-sham-env)


(sham-env-optimize-llvm! built-sham-env #:opt-level 2)
(sham-verify-llvm-ir built-sham-env)
(sham-print-llvm-ir built-sham-env)

(define mc-env (initialize-jit built-sham-env))

(module+ test
  ((jit-lookup-function mc-env 'print-foo)))
