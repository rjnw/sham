#lang racket

(require "../main.rkt")


#;(module+ test
  (require rackunit)

  (define apr (s$:tptr (s$:tref 'size-array)))
  (define mod
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      (s$:dtype (void) 'size-array (s$:tstruct '(size v) (list s$:i32 s$:i32*)))
      (s$:dfunction (void) 'create-empty-array (list 'size) (list s$:i32) apr
                    (s$:se
                     (s$:let '(ptr) (list apr) (list (s$:app^ (s$:rs 'malloc) (s$:etype (s$:tref 'size-array))))
                             (s$:block^
                              (s$:se (s$:app^ (s$:rs 'store!)
                                              (s$:v 'size)
                                              (s$:gep^ (s$:v 'ptr) (s$:ui32 0) (s$:ui32 0))))
                              (s$:se (s$:app^ (s$:rs 'store!)
                                              (s$:app^ (s$:rs 'arr-malloc) (s$:etype s$:i32) (s$:v 'size))
                                              (s$:gep^ (s$:v 'ptr) (s$:ui32 0) (s$:ui32 1))))
                              (s$:ret (s$:v 'ptr)))
                             (s$:evoid))))
      (s$:dfunction (void) 'array-get-element (list 'aptr 'ind) (list apr s$:i32) s$:i32
                    (s$:ret (s$:app^ (s$:rs 'load)
                                     (s$:gep^ (s$:app^ (s$:rs 'load) (s$:gep^ (s$:v 'aptr) (s$:ui32 0) (s$:ui32 1)))
                                              (s$:v 'ind)))))
      (s$:dfunction (void) 'array-store-element (list 'aptr 'ind 'v) (list apr s$:i32 s$:i32) s$:void
                    (s$:block^
                     (s$:se (s$:app^ (s$:rs 'store!)
                                     (s$:v 'v)
                                     (s$:gep^ (s$:app^ (s$:rs 'load) (s$:gep^ (s$:v 'aptr) (s$:ui32 0) (s$:ui32 1)))
                                              (s$:v 'ind))))
                     s$:ret-void))
      (s$:dfunction (void) 'free-array '(arr) (list apr) s$:void
                    (s$:block^
                     (s$:se (s$:app^ (s$:rs 'free)
                                     (s$:app^ (s$:rs 'load)
                                              (s$:gep^ (s$:v 'arr) (s$:ui32 0) (s$:ui32 1)))))
                     (s$:se (s$:app^ (s$:rs 'free)
                                     (s$:v 'arr)))
                     s$:ret-void)))))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (jit-verify-module mod-env)
  (optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)

  (define create-empty-array (jit-get-function 'create-empty-array mod-env))
  (define array-get-element (jit-get-function 'array-get-element mod-env))
  (define array-store-element (jit-get-function 'array-store-element mod-env))
  (define free-array (jit-get-function 'free-array mod-env))
  (define tarr (create-empty-array 10))
  (array-store-element tarr 5 42)
  (check-equal? (array-get-element tarr 5) 42)
  (free-array tarr))
