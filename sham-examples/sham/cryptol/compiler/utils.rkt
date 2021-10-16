#lang racket
(require (for-syntax syntax/parse))
(require "../ast.rkt"
         sham/sam/runtime/identifier)

(provide (all-defined-out))

(define (remove-gen-defs ds)
  (flatten
   (let loop ([defs ds])
     (cond [(list? defs) (map loop defs)]
           [(def-gen? defs) (loop (def-gen-ds defs))]
           [else defs]))))

(define (split-defs defs)
  (define val-defs (filter def-val? defs))
  (define type-defs (filter def-type? defs))
  (define typeof-defs (filter def-typeof? defs))
  (define test-defs (filter def-test? defs))
  (values val-defs type-defs typeof-defs test-defs))

(define (find-typeof name tdefs)
  (define (is-type? tdef)
    (match-define (def-typeof tnames ... t) tdef)
    (ormap (λ (tname) (free-identifier=? (ast-id-stxid name) (ast-id-stxid tname))) tnames))
  (findf is-type? tdefs))

(define (combine-val&typeof vdefs tdefs)
  (for/fold ([res '()])
            [(d vdefs)]
    (match-define (def-val name e) d)
    (cons (cons d (find-typeof name tdefs)) res)))


(define debug? (make-parameter #t))

(define-syntax (debug stx)
  (syntax-case stx ()
    [(_ es ...)
     #`(when (debug?) es ...)]))

(define (debug-print v) (debug (printf "debug: ") (pretty-print v) (newline)) v)

(define-syntax (TODO stx)
  (syntax-parse stx
    [(_ s:string args:expr ...)
     #`(error 'sham/cry/todo s args ...)]
    [else #`(error 'sham/cry/todo #,(format "~a" stx))]))
