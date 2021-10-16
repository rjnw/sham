#lang racket

(require "../ast.rkt"
         "ctxt.rkt"
         "utils.rkt")
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

(define (poly-apply-context cry-type ctxt)
  (match cry-type
    [(type-poly (vars ...) t)
     (make-type-poly vars
                     (poly-apply-context t (update-env ctxt #:type (for/list ([v vars]) (env-var v v #f)))))]
    [(type-constraint (cs ...) t) (make-type-constraint cs (poly-apply-context t ctxt))]
    [(type-sequence dim t) (type-sequence (poly-apply-context dim ctxt) (poly-apply-context t ctxt))]
    [(type-tuple ts ...) (make-type-tuple (map (curryr poly-apply-context ctxt) ts))]
    [(type-var name) (or (lookup-type ctxt name) cry-type)]
    [(type-func from to) (type-func (poly-apply-context from ctxt) (poly-apply-context to ctxt))]
    [(type-bit) cry-type]
    [(type-integer) cry-type]

    [(dim i) cry-type]
    [(dim-var name) (or (lookup-type ctxt name) cry-type)]
    [(dim-app rator rands ...) (make-dim-app rator (map (curryr poly-apply-context ctxt) rands))]))

(define (unknown-type? t ctxt)
  (match t
    [(type-var n) (unknown-type? (lookup-type ctxt n) ctxt)]
    [#f #t]
    [else #f]))
(define (poly-type? t ctxt)
  (match t
    [(type-poly (vs ...) t) #t]
    [(type-constraint (cs ...) t) (poly-type? t ctxt)]
    [else #f]))
(define (concrete-type? t ctxt)
  (and (not (unknown-type? t ctxt)) (not (poly-type? t ctxt))))

(define (unify-type t1 t2 ctxt)
  (debug (printf "unify-type: ~a ~a\n" t1 t2))
  (match* (t1 t2)
    [((type-var v1) (type-var v2))
     (debug (printf "both-vars: ~a ~a ~a ~a\n" v1 (lookup-type ctxt v1) v2 (lookup-type ctxt v2))
            (print-cc ctxt))
     #f]
    [((type-var n1) t2)
     #:when (not (unknown-type? t2 ctxt))
     (define nt1 (lookup-type ctxt n1))
     (cond [(not (unknown-type? nt1 ctxt)) (unify-type nt1 t2 ctxt)]
           [else (unify-type t2 t1 ctxt)])]
    [(#f t2) t2]
    [(t1 #f) t1]
    [((type-bit) _) t1]
    [((type-integer) (type-integer)) t1]
    [((type-integer) (type-sequence d (type-bit))) (type-sequence d (type-bit))]
    [((type-integer) (type-sequence d v)) #:when (unknown-type? v ctxt) (type-sequence d (type-bit))]
    [((type-tuple t1s ...) (type-tuple t2s ...))
     (make-type-tuple (map (curryr unify-type ctxt) t1s t2s))]
    [((type-tuple t1s ...) u) #:when (unknown-type? u ctxt) t1]
    [((type-poly (v1ars ...) pt1)
      t2)
     (unify-type pt1 t2 (update-env ctxt #:type (for/list ([v v1ars]) (env-var v #f #f))))]
    [((type-constraint (cs ...) ct1) t2) (unify-type ct1 t2 ctxt)]
    [((type-func ft1 tt1) (type-func ft2 tt2))
     (type-func (unify-type ft1 ft2 ctxt) (unify-type tt1 tt2 ctxt))]
    [((type-func ft1 ft2) t2) (unify-type t2 t1 ctxt)]
    [((type-sequence d1 t1) (type-sequence d2 t2))
     (type-sequence (unify-type d1 d2 ctxt) (unify-type t1 t2 ctxt))]
    [((dim-int i) (dim-var name)) (unify-type t1 (lookup-type ctxt name) ctxt)]
    [((dim-var n1) (dim-int i)) (unify-type t2 (lookup-type ctxt n1) ctxt)]
    [((dim-int i1) (dim-int i2))
     #:when (equal? i1 i2)
     t1]
    [((? dim-app?) t2)
     (unify-type (calc-dim t1 ctxt) t2 ctxt)]
    [(ut1 t2) #:when (unknown-type? ut1 ctxt) t2]
    [(t1 ut2) #:when (unknown-type? ut2 ctxt) t1]
    [(_ _)
     (debug (print-cc ctxt))
     (error 'sham/cryptol/unify "mismatch types: \n~a \n~a" t1 t2)]))

(define (maybe-sequence-elem-type seq-type)
  (match seq-type
    [(type-sequence dim t) t]
    [(type-poly (vs ...) t) (maybe-sequence-elem-type t)]
    [(type-constraint (cs ...) t) (maybe-sequence-elem-type t)]
    [else #f]))
(define (maybe-sequence-dim seq-type)
  (match seq-type
    [(type-sequence dim t) dim]
    [(type-poly (vs ...) t) (maybe-sequence-dim t)]
    [(type-constraint (cs ...) t) (maybe-sequence-dim t)]
    [else #f]))
(define (maybe-dim-i dim)
  (match dim
    [(dim-int i) i]
    [(dim-var name) TODO]
    [else #f]))

(define (maybe-tuple-type-i type i)
  (match type
    [(type-poly (vs ...) t) (maybe-tuple-type-i t i)]
    [(type-constraint (cs ...) t) (maybe-tuple-type-i t i)]
    [(type-tuple ts ...) (list-ref ts i)]
    [else #f]))

(define (maybe-tuple-type-length type)
  (match type
    [(type-poly (vs ...) t) (maybe-tuple-type-length t)]
    [(type-constraint (cs ...) t) (maybe-tuple-type-length t)]
    [(type-tuple ts ...) (length ts)]
    [else #f]))

(define (maybe-calc-type ast maybe-type ctxt)
  (debug (printf "maybe-calc-type: ~a ~a\n" ast maybe-type))
  (match ast
    [(expr-bind (ps ...) body) TODO ;; (maybe-calc-type body maybe-type ctxt)
     ]
    [(expr-app rator (tvars ...) vargs ...) TODO]
    [(expr-cond [chk thn] ... els) TODO]
    [(expr-var name) (unify-type (lookup-typeof ctxt name) maybe-type ctxt)]
    [(expr-tvar name) (unify-type (lookup-kind ctxt name) maybe-type ctxt)]
    [(expr-annot e t) (maybe-calc-type e (unify-type t maybe-type ctxt) ctxt)]
    [(expr-where body) (maybe-calc-type body maybe-type ctxt)]
    [(expr-lit i) (unify-type (type-integer) maybe-type ctxt)]
    [(expr-char c) (unify-type (type-sequence (dim-int 8) (type-bit)) maybe-type ctxt)]
    [(expr-tuple vs ...)
     (unify-type
      (make-type-tuple
       (map (λ (v t) (maybe-calc-type v t ctxt))
            vs
            (build-list (length vs) (λ (i) (maybe-tuple-type-i maybe-type i)))))
      maybe-type
      ctxt)]
    ;; [(expr-zero) (unify-type (type-integer) maybe-type ctxt)]

    [(expr-sequence-basic es ...)
     (define elem-type
       (for/fold [(t (maybe-sequence-elem-type maybe-type))]
                 [(e es)]
         (maybe-calc-type e t ctxt)))
     (define sdim (dim-int (length es)))
     (unify-type (type-sequence sdim elem-type) maybe-type ctxt)]
    [(expr-sequence-enum from step to)
     (define sdim
       (if (and (expr-lit? from) (expr-lit? step) (expr-lit? to))
           (/ (add1 (- (expr-lit-v to) (expr-lit-v from))) (expr-lit-v step))
           #f))
     (unify-type (type-sequence sdim (type-integer)) maybe-type ctxt)]
    [(expr-sequence-str s)
     (unify-type (type-sequence (dim-int (string-length s)) (type-sequence (dim-int 8) (type-bit))) maybe-type ctxt)]
    [(expr-sequence-comp body [(var val)] ...)
     (define valts (map (curryr maybe-calc-type #f ctxt) val))
     (define val-lengths (map (compose maybe-dim-i maybe-sequence-dim) valts))
     (define given-length (maybe-sequence-dim maybe-type))
     (unless (or given-length (andmap identity val-lengths))
       (error 'sham/cryptol "unknown length for sequence comprehension ~a ~a" ast val-lengths))
     (define dim (or given-length (dim-int (apply min val-lengths))))
     (define maybe-elem-type (maybe-sequence-elem-type maybe-type))
     (type-sequence
      dim
      (maybe-calc-type body maybe-elem-type
                       (update-env ctxt #:val (for/list ([vn var] [vt valts])
                                                (env-var vn #f (maybe-sequence-elem-type vt))))))]))

(define (kind-from-constraint name cs) (type-integer)) ;; TODO
(define (calc-dim-app da ctxt)
  (match-define (dim-app rator rands ...) da)
  (define rand-vals (map (compose maybe-dim-i (curryr calc-dim ctxt)) rands))
  (debug (printf "calc-dim-app: ~a ~a\n" da rand-vals))
  (define f
    (match (syntax->datum rator)
      [`+ +]
      [`- -]
      [`* *]
      [`/ /]
      [`/^ (compose ceiling /)]                                 ;; ceiling division
      [`%  modulo]
      [`%^ (compose ceiling modulo)]                               ;; ceiling modulas
      [`^^ expt]                               ;; exponentiation
      [`lg2 (compose ceiling (curryr log 2))]                              ;; ceiling log (base 2)
      [`width (compose ceiling (curryr log 2) add1)]                            ;; bit width (lg2 (n + 1))
      [`max max]
      [`min min]))
  (dim-int (apply f rand-vals)))

(define (calc-dim d ctxt)
  (match d
    [(dim-int i) d]
    [(dim-var n) (calc-dim (lookup-type ctxt n) ctxt)]
    [(? dim-app?) (calc-dim-app d ctxt)]))

(define (contains-var? t)
  (match t
    [(type-var _) #t]
    [(type-sequence d t) (and (contains-var? d) (contains-var? t))]
    [(type-tuple ts ...) (andmap contains-var? ts)]
    [(type-bit) #f]
    [(type-integer) #f]

    [(dim-var _) #t]
    [(dim-int i) #f]
    ))

(define (pargs-from-context! poly-type conc-type known-args ctxt)
  (define (just1 lst)
    (if (equal? (length lst) 1)
        (car lst)
        (error 'sham/sam/cry-type "poly vars incorrectly specified ~a ~a" lst known-args)))
  (define (get-known-val name)
    (define known-vals (lookup-env-vars known-args name))
    (just1 known-vals))
  (let rec ([pt poly-type] [ct conc-type])
    ;; (debug (printf "pargs-from-context: ~a ~a\n" pt ct))
    (match* (pt ct)
      [((type-func pf pt) (type-func cf ct)) (rec pf cf) (rec pt ct)]
      [((type-var pv) ct)
       (define known-env-var (get-known-val pv))
       (define new-type (unify-type (env-var-val known-env-var) ct ctxt))
       (if (unknown-type? new-type ctxt)
           (error 'sham/cryptol "cannot specialize: ~a" pv)
           (set-env-var-val! known-env-var new-type))]
      [((dim-var n) (dim-int i)) (set-env-var-val! (get-known-val n) ct)]
      [((dim-int i1) (dim-int i2)) #:when (equal? i1 i2) (void)]
      [((type-sequence pd pt) (type-sequence cd ct)) (rec pd cd) (rec pt ct)]
      [((type-tuple pts ...) (type-tuple cts ...)) (map rec pts cts)]
      [((type-bit) _) (void)]
      [((type-integer) _) (void)]
      [(t1 t2) #:when (not (contains-var? t1)) (void)]
      [(_ _) (TODO "pargs-from-context! ~a ~a" pt ct)]))
  known-args)

;; returns pvar-binds as env-vars
(define (figure-out-pargs type pargs varg-types res-type app-ctxt)
  (define-values (uptype pvars cs) (unwrap-poly type))
  ;; (debug (printf "figure-out-pargs:\n  type: ~a\n  pargs: ~a\n  varg-types: ~a\n  res-type: ~a\n" type pargs varg-types res-type))
  (define given-pvars
    (for/list ([pvar pvars]
               [i (length pvars)])
      (env-var pvar (if (< i (length pargs)) (list-ref pargs i) #f) (kind-from-constraint pvar cs))))
  ;; (debug (printf "given-pargs: ") (print-evs given-pvars))
  (pargs-from-context! uptype (foldr make-type-func res-type varg-types) given-pvars app-ctxt))

(define (specialize-func-type orig-type pvar-args maybe-varg-types maybe-result-type ctxt)
  (define maybe-type (foldr make-type-func maybe-result-type maybe-varg-types))
  (debug (printf "specializing-type: ~a ~a ~a\n" orig-type pvar-args maybe-type))
  (debug-print (unify-type orig-type maybe-type (update-env ctxt #:type pvar-args))))

(define (specialize-poly-type orig-type poly-args value-args ctxt)
  (define-values (uptype pvars cs) (unwrap-poly orig-type))
  (match-define (list varg-types ... res-type) (get-farg-types uptype))
  (define given-result-type (cc-type ctxt))
  (let* ([maybe-result-type (cc-type ctxt)]
         [maybe-varg-types (map (curryr maybe-calc-type ctxt) value-args (drop-right (get-farg-types uptype) 1))]
         [pvar-args (figure-out-pargs orig-type poly-args maybe-varg-types maybe-result-type ctxt)]
         [new-type (specialize-func-type uptype pvar-args maybe-varg-types maybe-result-type ctxt)])
    (debug (printf "specialized-poly-type: \n ~a\n ~a\n" orig-type new-type))
    (values pvar-args new-type)))
