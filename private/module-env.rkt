#lang racket

(require "env.rkt")

(provide (all-defined-out))
;; module info is mutable hash
;; as module env is immutable and we add stuff to module
;; info from time to time

;; function, type, and global info are immutable
;; which satisfy the invariant that each key maps to a list

(define info-sym '#%jit-info)
(define module-key 'module)
(define top-env-key 'top-env)
(define context-key 'context)
(define ffi-mapping-key 'ffi-mappings)
(define ffi-lib-key 'ffi-libs)
(define rkt-mapping-key 'rkt-mappings)
(define module-pass-info-key 'module-passes)
(define mcjit-info-key 'mcjit)
(define orc-info-key 'orc)
(define orc-handle-info-key 'orc-handle)
(define per-function-info-key 'per-function-info)
(define per-type-info-key 'per-type-info)

(define-syntax-rule (do-if-info-key key info v expr ...)
  (let ([v (get-info-key info key)])
    (when v
      expr ...)))

(define (empty-mod-env-info) (make-hash))

(define (update-info! info assocs)
  (for ([as assocs])
    (match-define (cons key val) as)
    (hash-set! info key val))
  info)

(define (get-info-key info key [failure-result #f])
  (hash-ref info key failure-result))
(define (set-info-key! info key val)
  (hash-set! info key val))

(define (env-get-info-key mod-env key [failure-result #f])
  (get-info-key (env-get-info mod-env) key failure-result))

(define (env-set-info-key! mod-env key val)
  (set-info-key! (env-get-info mod-env) key val))

(define (env-add-info mod-env info)
  (env-extend info-sym info mod-env))
(define (env-get-info mod-env)
  (env-lookup info-sym mod-env))

(define env-get-ffi-mappings (curryr env-get-info-key ffi-mapping-key))
(define env-get-rkt-mappings (curryr env-get-info-key rkt-mapping-key))
(define env-get-per-function-info-map (curryr env-get-info-key per-function-info-key))
(define env-get-per-type-info-map (curryr env-get-info-key per-type-info-key))

(define env-get-top-env (curryr env-get-info-key top-env-key))
(define env-set-top-env! (λ (mod-env env) (env-set-info-key! mod-env top-env-key env)))
(define env-get-llvm-module (curryr env-get-info-key module-key))
(define env-get-context (curryr env-get-info-key context-key))
(define env-get-mcjit (curryr env-get-info-key mcjit-info-key))
(define env-add-mcjit! (λ (mod-env mcjit) (env-set-info-key! mod-env mcjit-info-key mcjit)))
(define env-get-orc (curryr env-get-info-key orc-info-key))
(define env-get-orc-handle (curryr env-get-info-key orc-handle-info-key))
(define env-add-orc! (λ (mod-env orc) (env-set-info-key! mod-env orc-info-key orc)))
(define env-add-orc-handle! (λ (mod-env orc) (env-set-info-key! mod-env orc-handle-info-key orc)))

(define (mod-info-add-ffi-libs info . libs) ;libs is (cons libname ("libid" args))
  (define orig-libs (get-info-key info ffi-lib-key '()))
  (set-info-key! info ffi-lib-key (append orig-libs libs)))

(define (mod-info-add-passes info . passes)
  (set-info-key! info module-pass-info-key (append passes (hash-ref info module-pass-info-key '()))))

(define (get-module-passes mod-env)
  (env-get-info-key mod-env module-pass-info-key '()))
