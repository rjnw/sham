#lang racket

(require sham/ir/env
         sham/ir/dump
         sham/ir/verify
         sham/ir/builder
         sham/ir/optimize
         sham/ir/ast/core
         sham/ir/ast/simple
         sham/ir/ast/open
         sham/parameters
         sham/jit)

(require sham/private/keyword
         sham/private/env)
(require (for-syntax syntax/parse))

(provide (all-defined-out))

(define-general-keyword-procedure
  (sham-jit-compile! ka env/mod . rest-args)

  (define cm-env (sham-compile! env/mod ka (sham-compile-options)))
  (define jit-env (initialize-jit cm-env (lookup-keyword ka #:jit-type #:jit 'mc)))
  (env-try-set-callbacks! env/mod jit-env)
  jit-env)

(define (sham-compile! env/mod ka compile-options)
  (printf "compile! ~a ~a\n" ka compile-options)
  (define (compile-module mod)
    (define (has-compile-options? os)
      (ormap (λ (o) (member o compile-options)) os))
    (define (has-keyword-option? k)
      (assoc-default ka (string->keyword (to-string k)) #f))
    (define-syntax (when-option stx)
      (syntax-parse stx
        [(_ option:id body ...)
         #`(let ([option (or (has-compile-options? `(option))
                             (has-keyword-option? `option))])
             (when option
               body ...))]
        [(_ (options:id ...) body ...)
         #`(let ([option (has-compile-options? `(options ...))])
             (when option
               body ...))]))

    (when-option dump-sham (sham-dump-ir mod))
    (define e (build-sham-env mod))
    (when-option dump-llvm (sham-dump-llvm e))
    (when-option (dump-llvm-ir dump-llvm-ir-before-opt)
                 (sham-dump-llvm-ir e))
    (when-option verify-llvm-with-error
                 (sham-verify-llvm-ir-error e))
    (when-option opt-level
                 (sham-env-optimize-llvm! e #:opt-level opt-level))
    (when-option (dump-llvm-ir dump-llvm-ir-after-opt)
                 (sham-dump-llvm-ir e))
    e)
  (match env/mod
    [(? sham-env?) env/mod]
    [(? sham-module?) (compile-module env/mod)]
    [(? sham:def:module?) (sham-compile! (build-sham-module env/mod) ka compile-options)]
    [(? open-sham-env?) (compile-module (close-open-sham-env! env/mod))]))

(define (env-try-set-callbacks! env jit-env)
  (when (open-sham-env? env)
    (for ([v (open-sham-env-values env)]
          #:when (open-sham-function? v))
      (set-open-sham-function-compiled-app! v (jit-lookup-function jit-env (open-sham-function-id v))))))
