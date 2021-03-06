#lang racket

(require "base.rkt")

(provide (all-defined-out))
;; module info is mutable hash
;; as module env is immutable and we add stuff to module
;; info from time to time

;; function, type, and global info are immutable
;; which satisfy the invariant that each key maps to a list

(define info-sym '#%info)
(define llvm-module-key 'module)
(define top-env-key 'top-env)
(define context-key 'context)
(define ffi-mapping-key 'ffi-mappings)
(define ffi-lib-key 'ffi-libs)
(define rkt-mapping-key 'rkt-mappings)
(define module-early-pass-key 'early-module-pass)
(define module-late-pass-key 'late-module-pass)
(define mcjit-info-key 'mcjit)
(define orc-info-key 'orc)
(define orc-handle-info-key 'orc-handle)
(define per-function-info-key 'per-function-info)
(define per-type-info-key 'per-type-info)
(define call-conv-key 'call-conv)

(define-syntax-rule (do-if-info-key key info v expr ...)
  (let ([v (get-info-key info key)])
    (when v
      expr ...)))

(define (llvm-module-env? e)
  (and (hash? e)
       (hash-has-key? e llvm-module-key)))
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
(define env-get-llvm-module (curryr env-get-info-key llvm-module-key))
(define env-get-context (curryr env-get-info-key context-key))
(define env-get-mcjit (curryr env-get-info-key mcjit-info-key))
(define env-add-mcjit! (λ (mod-env mcjit) (env-set-info-key! mod-env mcjit-info-key mcjit)))
(define env-get-orc (curryr env-get-info-key orc-info-key))
(define env-get-orc-handle (curryr env-get-info-key orc-handle-info-key))
(define env-add-orc! (λ (mod-env orc) (env-set-info-key! mod-env orc-info-key orc)))
(define env-add-orc-handle! (λ (mod-env orc) (env-set-info-key! mod-env orc-handle-info-key orc)))

(define (env-add-late-module-pass! info . passes)
  (set-info-key! info module-late-pass-key (append passes (hash-ref info module-late-pass-key '()))))
(define (env-add-early-module-pass! info . passes)
  (set-info-key! info module-early-pass-key (append passes (hash-ref info module-early-pass-key '()))))

(define (env-get-late-module-passes mod-env)
  (env-get-info-key mod-env module-late-pass-key '()))
(define (env-get-early-module-passes mod-env)
  (env-get-info-key mod-env module-early-pass-key '()))
