#lang racket

(require sham/md
         sham/ir
         sham/llvm/ir/md
         sham/llvm/ir/env
         sham/jit
         sham/rkt
         sham/rkt/conv
         ffi/unsafe
         (prefix-in ll- sham/llvm/ir))

(define printf-em
  (external-mapping
   'printf
   (cast (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _fpointer) _fpointer _uintptr)
   ;; (ptr-ref (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _fpointer) _uintptr)
   ;; (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _uintptr)
   ))

(define printf-def (ll-def-external 'printf (ll-type-function (ll-type-pointer i8) #t i64)))

(define rkt-printf (λ (arg) (printf "rkt-print: ~a\n" arg) 0))
(define rkt-print (ll-def-external 'rkt-print (ll-type-function i64 #f i64)))

(define print-foo
  (function (print-foo : i64) (stmt-block
                               (stmt-expr (expr-app 'rkt-print (op-bitcast (ll-val-ref 'printf) (expr-etype i64))))
                               ;; (stmt-expr (expr-app 'rkt-print (ui64 42)))
                               (stmt-return (ui64 0)))))

(define sham-md
  (sham-module-metadata #:externals (list
                                     ;; printf-em
                                     (external-mapping 'rkt-print (func-uintptr rkt-printf (_fun _uint64 -> _uint64))))))

(define sham-module
  (def-module
    #:md sham-md
    "test-module"
    printf-def
    rkt-print
    print-foo))

(define built-sham-env (build-sham-env sham-module))
(sham-print-llvm-ir built-sham-env)

(sham-env-optimize-llvm! built-sham-env #:opt-level 2)
(sham-verify-llvm-ir built-sham-env)
(sham-print-llvm-ir built-sham-env)

(define mc-env (initialize-jit built-sham-env))

(define p1 (cast (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _fpointer) _fpointer _uintptr))
(define p2 (get-ffi-obj (string->bytes/locale "printf") #f _uintptr))
(define p3 (cast (get-ffi-obj (string->bytes/locale "printf") (ffi-lib #f) _pointer) _pointer _uintptr))
(define p4 (cast (ffi-obj-ref "printf" #f) _pointer _uintptr))
(define p5 (ptr-ref (cast p1 _uintptr _pointer) _uintptr))
(printf "printf-ptr-vals: ~a ~a ~a ~a ~a\n" p1 p2 p3 p4 p5)

(module+ test
  ((jit-lookup-function mc-env 'print-foo)))
