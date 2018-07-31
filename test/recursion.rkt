#lang racket

(require "../main.rkt")


(module+ test
  (require rackunit)

  (define mod
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      (s$:dfunction (void) 'meven? (list 'x) (list s$:i32) s$:i1
                    (s$:if (s$:app^ s$:icmp-eq
                                    (s$:app^ s$:urem (s$:v 'x) (s$:ui32 2))
                                    (s$:ui1  0))
                           (s$:ret (s$:ui1  1))
                           (s$:ret (s$:app^ (s$:rs 'modd?)
                                            (s$:app^ s$:sub (s$:v 'x) (s$:ui32 1))))))
      (s$:dfunction (void) 'modd? (list 'x) (list s$:i32) s$:i1
                    (s$:if (s$:app^ s$:icmp-eq
                                    (s$:app^ s$:urem (s$:v 'x) (s$:ui32 2))
                                    (s$:ui1 0))
                           (s$:ret (s$:ui1 0))
                           (s$:ret (s$:app^ (s$:rs 'meven?)
                                            (s$:app^ s$:sub (s$:v 'x) (s$:ui32 1)))))))))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (basic-optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)

  (define meven? (jit-get-function 'meven? mod-env))
  (check-eq? (meven? 42) 1)

  (define modd? (jit-get-function 'modd? mod-env))
  (check-eq? (modd? 42) 0))
