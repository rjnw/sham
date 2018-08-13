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
                                            (s$:app^ s$:sub (s$:v 'x) (s$:ui32 1))))))

      (s$:dfunction (void) 'factr '(x) (list s$:i64) s$:i64
                    (s$:se (s$:let '(result i) (list s$:i64 s$:i64) (list (s$:ui64 1) (s$:ui64 1))
                                   (s$:block^
                                    (s$:while
                                     (s$:app^ s$:icmp-ule (s$:v 'i) (s$:v 'x))
                                     (s$:block^
                                      (s$:set! (s$:v 'result)
                                               (s$:app^ s$:mul-nuw (s$:v 'result) (s$:v 'i)))
                                      (s$:set! (s$:v 'i)
                                               (s$:app^ s$:add-nuw (s$:v 'i) (s$:ui64 1)))))
                                    (s$:return (s$:v 'result)))
                                   (s$:evoid)))))))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (optimize-module mod-env #:opt-level 3 #:size-level 1 )
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)

  (define meven? (jit-get-function 'meven? mod-env))
  (check-eq? (meven? 42) 1)

  (define modd? (jit-get-function 'modd? mod-env))
  (check-eq? (modd? 42) 0)

  (define factr (jit-get-function 'factr mod-env))

  (check-eq? (factr 5) 120))
