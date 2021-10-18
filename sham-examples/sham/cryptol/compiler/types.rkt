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

#;(define (poly-apply-context cry-type ctxt)
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
    [(type-var n) (unknown-type? (lookup-tvar ctxt n) ctxt)]
    [#f #t]
    [else #f]))
(define (poly-type? t ctxt)
  (match t
    [(type-poly (vs ...) t) #t]
    [(type-constraint (cs ...) t) (poly-type? t ctxt)]
    [else #f]))
(define (concrete-type? t ctxt)
  (and (not (unknown-type? t ctxt)) (not (poly-type? t ctxt))))

(define (type-from-name ctxt n)
  (or (lookup-tvar ctxt n) (lookup-type ctxt n)))

(define (combine-unify-results . urs)
  (values (append-map car urs) (map cdr urs)))
(define (apply-uts f . args)
  (define evs (map car args))
  (define ts (map cdr args))
  (cons (apply append evs) (apply f ts)))

(define make-unified-result cons)
(define unified-vars car)
(define unified-type cdr)

;;  takes poly-type and maybe a concrete-type, tries to unify and also returns variable values figured
;; -> (cons new-type env-vars)
(define (unify-type t1 t2 ctxt)
  ;; (define (add-from-unify name ut ctxt)
  ;;   (match-define (cons vars t) ut)
  ;;   (cons (if (concrete-type? t ctxt) (cons (env-var name t) vars) vars) t))
  (define (add-type-var name tu ctxt)
    (cond [(and (cons? tu) (type? (cdr tu))) (make-unified-result
                                              (cons (env-var name (unified-type tu)) (unified-vars tu))
                                              (unified-type tu))]
          [(or (type? tu) (dim? tu)) (cons (list (env-var name tu)) tu)]
          [(false? tu) (no-vars tu)])
    ;; (add-from-unify name (cons '() t) ctxt)
    )
  (define (no-vars tu)
    (cond [(and (cons? tu) (type? (cdr tu))) tu]
          [(or (false? tu) (type? tu) (dim? tu)) (cons '() tu)]
          [else (error 'cry/unify "weird-result: ~a" tu)]))

  (debug (printf "unify-type: ~a ~a\n" (pretty-cry t1) (pretty-cry t2)))
  (match* (t1 t2)
    [((type-var v1) (type-var v2))
     (debug (printf "both-vars: ~a ~a ~a ~a\n" v1 (type-from-name ctxt v1) v2 (type-from-name ctxt v2))
            (print-cc ctxt))
     (cons '() #f)]
    [((type-var n1) t2)
     #:when (not (unknown-type? t2 ctxt))
     ;; (printf "n1n2: ~a ~a\n" (expand-type ctxt n1) (expand-type ctxt t2))
     (match (type-from-name ctxt n1)
       [#f (if t2 (add-type-var n1 t2 ctxt) t2)]
       [nt1 (unify-type nt1 t2 ctxt)])]
    [(t1 (type-var n2))
     #:when (not (unknown-type? t1 ctxt))
     (match (type-from-name ctxt n2)
       [#f (if t1 (add-type-var n2 t1 ctxt) t1)]
       [nt2 (unify-type t1 nt2 ctxt)])]
    [(#f t2) (no-vars t2)]
    [(t1 #f) (no-vars t1)]
    [((type-bit) _)  (no-vars t1)]
    [((type-integer) (type-integer)) (no-vars t1)]
    [((type-integer) (type-sequence d (type-bit))) (no-vars (type-sequence d (type-bit)))]
    [((type-integer) (type-sequence d v)) #:when (unknown-type? v ctxt) (no-vars (type-sequence d (type-bit)))]
    [((type-tuple t1s ...) (type-tuple t2s ...))
     (define t12u (map (curryr unify-type ctxt) t1s t2s))
     (cons (append-map car t12u)
           (make-type-tuple (map cdr t12u)))
     ;; (define t12u (map (curryr unify-type ctxt) t1s t2s))
     ;; (cons (append-map car t12u) (make-type-tuple (map cdr t12u)))
     ]
    [((type-tuple t1s ...) u) #:when (unknown-type? u ctxt) (no-vars t1)]
    [((type-poly (v1ars ...) pt1) t2)
     (match-define (cons ut-vars utype) (unify-type pt1 t2 (update-env ctxt #:tvar (for/list ([v v1ars]) (env-var v #f)))))
     ;; TODO
     (cons (filter-not (λ (v) (member (env-var-name v) v1ars)) ut-vars) ;; TODO remove only first
           utype)]
    [((type-constraint (cs ...) ct1) t2) (unify-type ct1 t2 ctxt)]
    [((type-func ft1 tt1) (type-func ft2 tt2))
     (apply-uts make-type-func (unify-type ft1 ft2 ctxt) (unify-type ft1 ft2 ctxt))
     ;; (define ftu (unify-type ft1 ft2 ctxt))
     ;; (define ttu (unify-type tt1 tt2 ctxt))
     ;; (cons (append (car ftu) (car ttu)) (type-func (cdr ftu) (cdr ttu)))
     ]
    ;; [((type-func ft1 ft2) t2) (unify-type t2 t1 ctxt)]
    [((type-sequence d1 t1) (type-sequence d2 t2))
     (apply-uts make-type-sequence (unify-type d1 d2 ctxt) (unify-type t1 t2 ctxt))
     ;; (define dtu (unify-type d1 d2 ctxt))
     ;; (define ttu (unify-type t1 t2 ctxt))
     ;; (cons (append (car dtu) (cdr ttu)) (type-sequence (cdr dtu) (cdr ttu)))
     ]
    [((dim-var n1) (dim-int i)) (add-type-var n1 (unify-type t2 (type-from-name ctxt n1) ctxt) ctxt)]
    [((dim-var n1) t2)
     ;; (printf "lt: ~a\n" (type-from-name ctxt n1))
     (cond [(type-from-name ctxt n1) => (λ (t) (add-type-var n1 (unify-type t t2 ctxt) ctxt))]
           [(equal? t1 t2) (no-vars t1)]
           [else (TODO "dim-var: ~a ~a\n" n1 t2)])]
    [((dim-int i) (dim-var n2)) (add-type-var n2 (unify-type t1 (type-from-name ctxt n2) ctxt) ctxt)]
    [((dim-int i1) (dim-int i2))
     #:when (equal? i1 i2)
     (no-vars t1)]
    [((? dim-app?) t2)
     (unify-type (calc-dim t1 ctxt) t2 ctxt)]
    [(t1 t2) #:when (equal? t1 t2) (no-vars t1)]
    ;; [(ut1 t2) #:when (unknown-type? ut1 ctxt) t2]
    ;; [(t1 ut2) #:when (unknown-type? ut2 ctxt) t1]
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

;; TODO cache unified/calculated types
(define (type-from-md ast) #f)
(define (update-type-md! ast type) type)

;; returns similar result as unify-type: (cons vars type)
(define (maybe-calc-type ast maybe-type^ ctxt)
  (debug (printf "maybe-calc-type: ~a ~a\n" (pretty-cry ast) (pretty-cry maybe-type^)))
  (define maybe-utype (unify-type (type-from-md ast) maybe-type^ ctxt))
  (define maybe-type (cdr maybe-utype))
  (define (calc-type)
    (match ast
      [(expr-bind (ps ...) body) TODO ;; (maybe-calc-type body maybe-type ctxt)
                                 ]
      [(expr-app rator (pargs ...) vargs ...)
       (define rator-type (lookup-typeof ctxt (expr-var-name rator)))
       (define-values (pvars new-type) (specialize-poly-type rator-type pargs vargs ctxt))
       (unify-type (last (get-farg-types new-type)) maybe-type ctxt)]
      [(expr-cond [chk thn] ... els) TODO]
      [(expr-var name) (unify-type (lookup-typeof ctxt name) maybe-type ctxt)]
      [(expr-tvar name) (unify-type (type-from-name ctxt name) maybe-type ctxt)]
      [(expr-annot e t) (maybe-calc-type e (unify-type t maybe-type ctxt) ctxt)]
      [(expr-where body) (maybe-calc-type body maybe-type ctxt)]
      [(expr-lit i) (unify-type (type-integer) maybe-type ctxt)]
      [(expr-char c) (unify-type (type-sequence (dim-int 8) (type-bit)) maybe-type ctxt)]
      [(expr-tuple vs ...)
       (define vtus
         (map (λ (v t) (maybe-calc-type v t ctxt))
              vs
              (build-list (length vs) (λ (i) (maybe-tuple-type-i maybe-type i)))))
       (debug (printf "throwing-away: ~a\n" (map car vtus)))
       (unify-type (make-type-tuple (map cdr vtus)) maybe-type ctxt)]
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
       (unify-type (type-sequence (dim-int (string-length s))
                                  (type-sequence (dim-int 8) (type-bit)))
                   maybe-type ctxt)]
      [(expr-sequence-comp body [(var val)] ...)
       (define-values (val-tvars valts) (apply combine-unify-results (map (curryr maybe-calc-type #f ctxt) val)))
       (define val-lengths (map (compose maybe-dim-i maybe-sequence-dim) valts))
       (define given-length (maybe-sequence-dim maybe-type))
       (unless (or given-length (andmap identity val-lengths))
         (error 'sham/cryptol "unknown length for sequence comprehension ~a ~a" ast val-lengths))
       (define dim (or given-length (dim-int (apply min val-lengths))))
       (define maybe-dim-type (cons '() dim))
       (define maybe-elem-type
         (maybe-calc-type body (maybe-sequence-elem-type maybe-type)
                          (update-env ctxt
                                      ;; #:type val-tvars
                                      #:typeof (for/list ([vn var] [vt valts])
                                                 (env-var vn (maybe-sequence-elem-type vt))))))
       (apply-uts make-type-sequence maybe-dim-type maybe-elem-type)]))
  (update-type-md! ast ;; (if (concrete-type? maybe-type ctxt) maybe-utype (calc-type)) ;; TODO do deep poly check for concrete-type?
                   (calc-type)
                   ))

(define (kind-from-constraint name cs) (type-integer)) ;; TODO
(define (calc-dim-app da ctxt)
  (match-define (dim-app rator rands ...) da)
  (define rand-vals (map (compose maybe-dim-i (curryr calc-dim ctxt)) rands))
  (debug (printf "calc-dim-app: ~a ~a\n" (pretty-cry da) rand-vals))
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
  (if (ormap false? rand-vals) #f (dim-int (apply f rand-vals))))

(define (calc-dim d ctxt)
  (debug (printf "calc-dim: ~a\n" (pretty-cry d)))
  (match d
    [(dim-int i) d]
    [(dim-var n) (calc-dim (type-from-name ctxt n) ctxt)]
    [(? dim-app?) (calc-dim-app d ctxt)]
    [#f #f]))

(define (contains-var? t)
  (match t
    [(type-var _) #t]
    [(type-sequence d t) (and (contains-var? d) (contains-var? t))]
    [(type-tuple ts ...) (andmap contains-var? ts)]
    [(type-bit) #f]
    [(type-integer) #f]

    [(dim-var _) #t]
    [(dim-int i) #f]))

;; TODO kind of pvars
;; (kind-from-constraint pvar cs)
;; tries to match given poly type args with needed poly type vars
(define (given-pvars pvars pargs cs)
  (for/list ([pvar pvars]
             [i (length pvars)])
    (env-var pvar (if (< i (length pargs)) (list-ref pargs i) #f))))

(define (specialize-poly-type orig-type poly-args value-args ctxt)
  (debug (printf "specialize-poly-type: ~a\npargs: ~a\nvargs: ~a\n"
                 (pretty-cry orig-type) (map pretty-cry poly-args) (map pretty-cry value-args)))
  (define-values (uptype pvars cs) (unwrap-poly orig-type))
  (match-define (list varg-types^ ... res-type) (get-farg-types uptype))
  (define given-result-type (cc-type ctxt))
  (define pevs (given-pvars pvars poly-args cs))
  (define pctxt (update-env ctxt #:tvar pevs))

  (define res-ut-type (unify-type res-type given-result-type pctxt))
  (debug (printf "spt: res-type:~a\n" (pretty-cry res-ut-type)))
  (define-values (varg-ctxt varg-types)
    (for/fold ([ctxt (update-env pctxt #:tvar (car res-ut-type))]
               [vts '()])
              ([v value-args]
               [t varg-types^])
      (define v-ut (maybe-calc-type v t ctxt))
      (define t-ut (unify-type (cdr v-ut) t ctxt))
      (values (update-env ctxt #:tvar (append (car v-ut) (car t-ut)))
              (cons (cdr t-ut) vts))))
  (define pargs-vals (for/list ([p pevs]) (lookup-tvar varg-ctxt (env-var-name p))))
  (debug (printf "specialized-poly: ~a ~a ~a\n" (map pretty-cry varg-types) pevs pargs-vals))
  (when (ormap false? pargs-vals)
    (print-cc varg-ctxt)
    (error 'sham/cry "cannot specialize: ~a ~a ~a\n" (pretty-cry orig-type) pargs-vals varg-types))

  (values (map (λ (p pv) (env-var (env-var-name p) pv)) pevs pargs-vals)
          (foldr make-type-func (cdr res-ut-type) (reverse varg-types)))
  #;(let* ([maybe-result-type (cc-type ctxt)]
         [maybe-varg-types (map (curryr maybe-calc-type ctxt) value-args (drop-right (get-farg-types uptype) 1))]
         [pvar-args (figure-out-pargs orig-type poly-args maybe-varg-types maybe-result-type ctxt)]
         [new-type (specialize-func-type uptype pvar-args maybe-varg-types maybe-result-type ctxt)])
    (debug (printf "specialized-poly-type: \n ~a\n ~a\n" orig-type new-type))
    (values pvar-args new-type)))
