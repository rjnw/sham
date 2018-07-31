#lang racket

(require "../main.rkt")


(module+ test
  (require rackunit)

  (define mod
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      (s$:dfunction (void) 'create-box '() '() s$:i32*
                    (s$:ret (s$:app^ (s$:rs 'malloc)
                                     (s$:etype s$:i32))))
      (s$:dfunction (void) 'store-box (list 'x 'box) (list s$:i32 s$:i32*) s$:void
                    (s$:block^
                     (s$:se (s$:app^ (s$:rs 'store!)
                                     (s$:v 'x)
                                     (s$:v 'box)))
                     s$:ret-void))
      (s$:dfunction (void) 'open-box (list 'x) (list s$:i32*) s$:i32
                    (s$:ret (s$:app^ (s$:rs 'load)
                                     (s$:v 'x))))
      (s$:dfunction (void) 'free-box (list 'x) (list s$:i32*) (s$:tref 'void)
                    (s$:block^
                     (s$:se (s$:app^ (s$:rs 'free)
                                     (s$:v 'x)))
                     s$:ret-void)))))

  (define mod-env (compile-module mod))
  (jit-dump-module mod-env)
  (basic-optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (initialize-jit! mod-env)

  (define create-box (jit-get-function 'create-box mod-env))
  (define store-box (jit-get-function 'store-box mod-env))
  (define open-box (jit-get-function 'open-box mod-env))
  (define free-box (jit-get-function 'free-box mod-env))

  (define tbox (create-box))
  (store-box 42 tbox)
  (check-eq? (open-box tbox) 42)
  (free-box tbox))
