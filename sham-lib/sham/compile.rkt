#lang racket

(require sham/ir
         sham/ir/env
         sham/parameters
         sham/jit)

(require sham/private/keyword)
(require (for-syntax syntax/parse))

(define-general-keyword-procedure
  (sham-jit-compile! ka env/mod . rest-args)

  (define cm-env (sham-compile! env/mod (lookup-keyword ka #:compile-options #:options '())))
  (define jit-env (initialize-jit cm-env (lookup-keyword ka #:jit-type #:jit 'mc)))
  (env-try-set-callbacks! env/mod cm-env jit-env)
  jit-env)

(define (sham-compile! env/mod compile-options)
  (define (compile-module mod)
    (define e (build-sham-env mod))
    (define (has-options? os)
      (ormap (λ (o) (member o compile-options)) os))
    (define-syntax (when-option stx)
      (syntax-parse stx
        [(_ option:id body ...)
         #`(let ([option (has-options? `(option))])
             (when option
               body ...))]
        [(_ ((~literal or) options:id ...) body ...)
         #`(let ([option (has-options? `(options ...))])
             (when option
               body ...))]))
    (when-option (or dump-llvm dump-llvm-before-opt)
                 (sham-dump-llvm-ir e))
    (when-option opt-level
                 (sham-env-optimize-llvm! e #:opt-level opt-level))
    (when-option (or dump-llvm dump-llvm-after-opt)
                 (sham-dump-llvm-ir e)))
  (match env/mod
    [(? sham-env?) env/mod]
    [(? sham-module?) (compile-module env/mod)]
    [(? sham:def:module?) (sham-compile! (build-sham-module env/mod))]
    [(? open-sham-env?) (compile-module (close-open-sham-env! env/mod))]))

(define (env-try-set-callbacks! env jit-env)
  (when (open-sham-env? env)
    (for ([of (open-sham-env-opens env)])
      (set-open-sham-function-closed! of (jit-lookup-function jit-env (open-sham-function-id of))))))
