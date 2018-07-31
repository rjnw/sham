#lang racket

(require "../main.rkt")

(define mod
  (s$:dmodule
   (empty-mod-env-info) 'test-type
   (list
    (s$:dtype (void) 'size-array (s$:tstruct '(size v) (list s$:i32 s$:i32*)))
    (s$:dtype (void) 'array-len10 (s$:tarr s$:i32 10))
    (s$:dtype (void) 'vector-len4 (s$:tvec s$:i32 4)))))

(define mod-env (compile-module mod))
(jit-dump-module mod-env)
