#lang racket

(require "../ast.rkt")
(provide (all-defined-out))

(define (get-farg-types cry-type)
  (match cry-type
    [(type-poly (vars ...) t) (get-farg-types t)]
    [(type-constraint (cs ...) t) (get-farg-types t)]
    [(type-func frm to) (cons frm (get-farg-types to))]
    [else (list cry-type)]))

(define (unwrap-poly cry-type (vs '()) (cs '()))
  (match cry-type
    [(type-poly (vars ...) t) (unwrap-poly t (append vs vars) cs)]
    [(type-constraint (cns ...) t) (unwrap-poly t vs (append cs cns))]
    [else (values cry-type vs cs)]))
