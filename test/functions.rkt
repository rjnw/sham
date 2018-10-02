#lang racket

(require "../main.rkt")


(module+ test
  (require rackunit)

  (define mod
    (s$:dmodule
     (empty-mod-env-info) 'test-mod
     (list
      (s$:dfunction (void) 'const42 '() '() s$:i32
                    (s$:ret (s$:ui32 42)))
      (s$:dfunction (void) 'identity (list 'v) (list s$:i32) s$:i32
                    (s$:ret (s$:v 'v)))
      (s$:dfunction (void) 'even? (list 'x) (list s$:i32) s$:i32
                    (s$:if (s$:icmp-eq (s$:urem (s$:v 'x) (s$:ui32 2))
                                       (s$:ui32 0))
                           (s$:ret (s$:ui32 1))
                           (s$:ret (s$:ui32 0))))
      )))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)

  (define const42 (jit-get-function 'const42 mod-env))
  (check-eq? (const42) 42)

  (define id (jit-get-function 'identity mod-env))
  (check-eq? (id 42) 42)

  (define even-huh (jit-get-function 'even? mod-env))
  (check-eq? (even-huh 42) 1))
