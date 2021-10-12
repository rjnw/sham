#lang racket
(require "../ast.rkt"
         sham/sam/runtime/identifier)

(provide (all-defined-out))
(define (remove-gen-defs defs)
  (append-map (λ (d) (if (def-gen? d) (def-gen-ds d) d)) defs))

(define (split-defs defs)
  (define val-defs (filter def-val? defs))
  (define type-defs (filter def-type? defs))
  (define typeof-defs (filter def-typeof? defs))
  (define test-defs (filter def-test? defs))
  (values val-defs type-defs typeof-defs test-defs))

(define (find-typeof name tdefs)
  (define (is-type? tdef)
    (match-define (def-typeof tname t) tdef)
    (free-identifier=? (ast-id-stxid name) (ast-id-stxid tname)))
  (findf is-type? tdefs))

(define (combine-val&typeof vdefs tdefs)
  (for/fold ([res '()])
            [(d vdefs)]
    (match-define (def-val name e) d)
    (cons (cons d (find-typeof name tdefs)) res)))
