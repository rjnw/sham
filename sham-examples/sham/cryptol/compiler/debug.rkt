#lang racket
(require racket/trace
         (for-syntax syntax/parse))
(require "utils.rkt"
         "ctxt.rkt"
         "../ast.rkt")
(provide (all-defined-out)
         (all-from-out racket/trace))

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

(define (prettyify val)
  (match val
    [(? struct-cry-ast?) (pretty-cry val)]
    [else val]))

(define (trace-print-args fname vals kws kw-vals depth)
  (printf "~a> ~a: " (build-string depth (const #\ )) fname)
  (match fname
    [(or 'maybe-calc-type 'unify-type) (printf "~a - ~a\n" (pretty-cry (first vals)) (pretty-cry (second vals)))]
    ['specialize-poly-type (printf "~a:~a ~a -> ~a\n"
                                   (pretty-cry (first vals))
                                   (map pretty-cry (second vals))
                                   (map pretty-cry (third vals))
                                   (pretty-cry (cc-type (fourth vals))))]
    [else (pretty-print (map prettyify vals))]))

(define (trace-print-results fname results depth)
  (printf "~a< ~a: " (build-string depth (const #\ )) fname)
  (define (type-vars vs)
    (for/list ([v vs])
      (printf " ~a:~a " (env-var-name v) (pretty-cry (env-var-val v)))))
  (match fname
    [(or 'unify-type 'maybe-calc-type) (printf "~a $" (pretty-cry (cdr (first results))))
                                       (type-vars (car (first results))) (newline)]
    ['specialize-poly-type (printf "~a :" (pretty-cry (second results)))
                           (type-vars (first results)) (newline)]
    [else (pretty-print (map prettyify results))]))

(current-trace-print-args trace-print-args)
(current-trace-print-results trace-print-results)
