#lang racket

(require "../main.rkt")
(require disassemble)
(module+ test
  (require rackunit)

  (define mod
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      (s$:dtype (void) 'vi4 (s$:tvec s$:i32 4))
      (s$:dfunction (void) 'evec '() '() (s$:tref 'vi4)
                    (s$:ret (s$:vec (s$:ui32 0) (s$:ui32 0) (s$:ui32 0) (s$:ui32 0))))
      (s$:dfunction (void) 'bv (list 'i0 'i1 'i2 'i3) (list s$:i32 s$:i32 s$:i32 s$:i32) (s$:tref 'vi4)
                    (s$:se
                     (s$:let '(tv)
                             (list (s$:tref 'vi4))
                             (list (s$:app^ (s$:rs 'evec)))
                             (s$:block^
                              (s$:set! (s$:v 'tv) (s$:app^ (s$:rs 'insertelement) (s$:v 'tv) (s$:v 'i0) (s$:ui32 0)))
                              (s$:set! (s$:v 'tv) (s$:app^ (s$:rs 'insertelement) (s$:v 'tv) (s$:v 'i1) (s$:ui32 1)))
                              (s$:set! (s$:v 'tv) (s$:app^ (s$:rs 'insertelement) (s$:v 'tv) (s$:v 'i2) (s$:ui32 2)))
                              (s$:set! (s$:v 'tv) (s$:app^ (s$:rs 'insertelement) (s$:v 'tv) (s$:v 'i3) (s$:ui32 3)))
                              (s$:ret (s$:v 'tv)))
                             (s$:evoid))))
      (s$:dfunction (void) 'test-sum '(i0 i1 i2 i3 j0 j1 j2 j3)
                    (list s$:i32 s$:i32 s$:i32 s$:i32
                          s$:i32 s$:i32 s$:i32 s$:i32)
                    s$:i32

                    (s$:se
                     (s$:let '(v1 v2 v3)
                             (list (s$:tref 'vi4) (s$:tref 'vi4) (s$:tref 'vi4))
                             (list  (s$:app^ (s$:rs 'evec)) (s$:app^ (s$:rs 'evec)) (s$:app^ (s$:rs 'evec)))
                             (s$:block^
                              (s$:set! (s$:v 'v1) (s$:app^ (s$:rs 'insertelement) (s$:v 'v1) (s$:v 'i0) (s$:ui32 0)))
                              (s$:set! (s$:v 'v1) (s$:app^ (s$:rs 'insertelement) (s$:v 'v1) (s$:v 'i1) (s$:ui32 1)))
                              (s$:set! (s$:v 'v1) (s$:app^ (s$:rs 'insertelement) (s$:v 'v1) (s$:v 'i2) (s$:ui32 2)))
                              (s$:set! (s$:v 'v1) (s$:app^ (s$:rs 'insertelement) (s$:v 'v1) (s$:v 'i3) (s$:ui32 3)))

                              (s$:set! (s$:v 'v2) (s$:app^ (s$:rs 'insertelement) (s$:v 'v2) (s$:v 'j0) (s$:ui32 0)))
                              (s$:set! (s$:v 'v2) (s$:app^ (s$:rs 'insertelement) (s$:v 'v2) (s$:v 'j1) (s$:ui32 1)))
                              (s$:set! (s$:v 'v2) (s$:app^ (s$:rs 'insertelement) (s$:v 'v2) (s$:v 'j2) (s$:ui32 2)))
                              (s$:set! (s$:v 'v2) (s$:app^ (s$:rs 'insertelement) (s$:v 'v2) (s$:v 'j3) (s$:ui32 3)))

                              (s$:set! (s$:v 'v3) (s$:app^ s$:add (s$:v 'v1) (s$:v 'v2)))
                              (s$:ret (s$:app^ s$:add
                                               (s$:app^ (s$:rs 'extractelement) (s$:v 'v3) (s$:ui32 0))
                                               (s$:app^ s$:add
                                                        (s$:app^ (s$:rs 'extractelement) (s$:v 'v3) (s$:ui32 1))
                                                        (s$:app^ s$:add
                                                                 (s$:app^ (s$:rs 'extractelement) (s$:v 'v3) (s$:ui32 2))
                                                                 (s$:app^ (s$:rs 'extractelement) (s$:v 'v3) (s$:ui32 3)))))))
                             (s$:evoid)))))))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (jit-verify-module mod-env)
  (basic-optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)

  (define bv (jit-get-function 'bv mod-env))
  (define sv (jit-get-function 'test-sum mod-env))
  (disassemble-ffi-function (jit-get-function-ptr 'test-sum mod-env) #:size 100)
  (check-eq? (sv 1 1 1 1 2 2 2 2) 12))
