#lang racket

(require "module-env.rkt")
(provide (all-defined-out))

(define (basic-empty-info)
  (make-immutable-hash))

(define (get-info-list info key)
  (hash-ref info key '()))
(define (add-to-info-list info key value)
  (hash-set info key (cons value (get-info-list info key))))
(define (add-list-to-info-list info key value-list)
  (hash-set info key (append (get-info-list info key) value-list)))

;; module info
(define empty-module-info basic-empty-info)
(define (module-info-add-ffi-libs info . libs) ;libs is (cons libname ("libid" args))
  (define orig-libs (get-info-key info ffi-lib-key '()))
  (add-list-to-info-list info ffi-lib-key (append orig-libs libs)))
(define (module-info-add-late-pass mod-info . passes)
  (add-list-to-info-list mod-info module-late-pass-key passes))
(define (module-info-add-early-pass mod-info . passes)
  (add-list-to-info-list mod-info module-early-pass-key passes))

;; linked modules
(define module-info-link-module-key 'linked-modules)
(define module-info-get-link-module
  (curryr get-info-list module-info-link-module-key))
(define (module-info-add-link-module mod-info module-env)
  (add-to-info-list mod-info module-info-link-module-key module-env))


;; function info
(define empty-function-info basic-empty-info)
(define function-info-attribute-key 'attribute)
(define function-info-return-attribute-key 'return-attribute)
(define (function-info-argument-attribute-key n) `(argument-attribute ,n))
(define function-info-metadata-key 'metadata)
(define function-info-pass-key 'pass)
(define function-attribute-index (modulo -1 (expt 2 32)))


(define (function-info-add-attributes info . attributes)
  (add-list-to-info-list info function-info-attribute-key attributes))
(define (function-info-add-ret-attributes info . attributes)
  (add-list-to-info-list info function-info-return-attribute-key attributes))
(define (function-info-add-argument-attributes info argument-number . attributes)
  (add-list-to-info-list info (function-info-argument-attribute-key argument-number) attributes))
(define (function-info-add-metadata info . metadatas)
  (add-list-to-info-list info function-info-metadata-key metadatas))
(define (function-info-add-passes info . passes)
  (add-list-to-info-list info function-info-pass-key passes))

;; type info
(define empty-type-info basic-empty-info)
