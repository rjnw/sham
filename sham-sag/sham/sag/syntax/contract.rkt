#lang racket

(require
 (for-syntax "generics.rkt"
             "spec.rkt"
             syntax/parse
             racket/syntax
             racket/list
             racket/match
             racket/pretty)
 "runtime.rkt")

(begin-for-syntax
  (define ((do-pat for-id) p)
    (match p
      [(ast:pat:single sid) (for-id sid)]
      [(ast:pat:datum syn) #f]
      [(ast:pat:multiple ps) #`(vector/c #,@(filter-map (do-pat for-id) ps))]
      [(ast:pat:repeat pr) #`(listof #,((do-pat for-id) pr))]
      [(ast:pat:checker check cid) #`(flat-contract #,check)]))

  (struct rkt-struct-contract [id-fmt]
    #:methods gen:ast-builder
    [(define (build-top-struct ab tstruct as) tstruct)
     (define (build-group-struct ab gstruct as gs) gstruct)
     (define (build-group-extra ab gextra as gs)
       (match-define (rkt-struct-contract id-fmt) ab)
       (match-define (ast:group gid gsyn-id gparent gargs gnodes ginfo) gs)
       (with-syntax ([gname (id-fmt gid gsyn-id)]
                     [(node/c ...)
                      (map id-fmt
                           (map ast:basic-id (hash-values gnodes))
                           (map ast:basic-syn-id (hash-values gnodes)))])
         (cons #`(define gname (or/c node/c ...)) gextra)))
     (define (build-node-struct ab nstruct as gs ns) nstruct)
     (define (build-node-extra ab nextra as gs ns)
       (match-define (rkt-struct-contract id-fmt) ab)
       (match-define (ast:group gid gsyn-id gparent gargs gnodes ginfo) gs)
       (match-define (ast:node nid nsyn-id args pat info) ns)
       (define (id-type id)
         (match (findf (lambda (a) (equal? id (ast:basic-id a))) args)
           [(ast:node:arg id sid t info)
            (cond [(ast:type:internal? t)
                   (let ([of (ast:type:internal-of t)]) (id-fmt of of))]
                  [else #`any/c])]
           [else #`any/c]))
       (with-syntax ([cname (id-fmt nid nsyn-id)])
         (cons #`(define cname (flat-named-contract `#,nid #,((do-pat id-type) pat)) )
               nextra)))])

  (module+ test
    (require rackunit)
    (define pt
      (ast:pat:multiple
       (list
        (ast:pat:datum `letrec)
        (ast:pat:multiple
         (list
          (ast:pat:repeat
           (ast:pat:multiple
            (list (ast:pat:single #'ids) (ast:pat:single #'vals))))))
        (ast:pat:single #'e))))
    (check-equal? (syntax->datum ((do-pat (lambda (x) x)) pt)) `(vector/c (vector/c (listof (vector/c ids vals))) e))))

(define-syntax (rkt-contracts ast-spec)
  (rkt-struct-contract (lambda (i s) (format "~a/c" i s))))
