#lang racket
(require (prefix-in pp: pprint)
         racket/generic)

(require "runtime.rkt"
         "runtime/ast.rkt"
         "runtime/identifier.rkt")

(provide (all-defined-out))

(define-generics pretty-doc
  (get-doc pretty-doc))

(define-generics pretty-sexp
  (get-sexp pretty-sexp))

(define (pretty-print-ast a (out (current-output-port)) (width #f))
  (define (pp v)
    (parameterize ([pretty-print-columns (or width (pretty-print-columns))])
               (pretty-print v out)))
  (let rec [(v a)]
      (cond [(pretty-doc? v)
             (pp:pretty-print (get-doc v) out (or width (pp:current-page-width)))]
            [(pretty-sexp? v) (pp (get-sexp v))]
            [(ast:term? v)
             (match-define (ast:term md gargs nargs) v)
             (fprintf out "(~a " (object-name v) )
             (rec gargs)
             (rec nargs)
             (fprintf out ")")]
            [(ast:id? v) (print (syntax->datum (ast:id-stxid v)) out)]
            [(vector? v) (vector-map rec v)]
            [(list? v) (map rec v)]
            [else
             (parameterize ([pretty-print-columns (or width (pretty-print-columns))])
               (print v out))])))
