#lang racket
(require "../libjit.rkt")
(require "../jit.rkt")
(require ffi/unsafe)

(define f
  (cast (get-function-closure
         (compile-exp


          `(define (add (x : ,jit_type_uint) (y : ,jit_type_uint) : ,jit_type_uint)
             (define-var (z : ,jit_type_uint)
               (assign z 10)
               (return (- (+ x (+ z y)) 1))))))



        _pointer
        (_fun jit_uint jit_uint -> jit_uint)))
(f 2 3)
