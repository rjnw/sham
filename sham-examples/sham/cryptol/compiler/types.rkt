#lang racket
(require "../ast.rkt"
         "ctxt.rkt"
         "utils.rkt"
         "debug.rkt")
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

;; returns type with substituted poly vars from ctxt, if contains a type-poly or unknown var then #f
(define (maybe-full-type cry-type ctxt)
  (let rec ([t cry-type])
    (match t
      [#f #f]
      [(type-poly (vars ...) t) #f]
      [(type-constraint (cs ...) t)
       (define t^ (rec t))
       (and t^ (make-type-constraint cs t^))]
      [(type-sequence dim t)
       (define dim^ (rec dim))
       (define t^ (rec t))
       (and dim^ t^ (type-sequence dim^ t^))]
      [(type-tuple ts ...)
       (define ts^ (map rec ts))
       (and (andmap identity ts^) (make-type-tuple ts^))]
      [(type-var name)
       (define t^ (lookup-tvar ctxt name))
       (rec t^)]
      [(type-func from to)
       (define from^ (rec from))
       (define to^ (rec to))
       (and from^ to^ (type-func from^ to^))]
      [(type-bit) t]
      [(type-integer) #f]

      [(dim i) t]
      [(dim-var name) (rec (lookup-tvar ctxt name))]
      [(dim-app rator rands ...)
       (define rands^ (map rec rands))
       (and (andmap identity rands^) (make-dim-app rator rands^))])))

(define (unknown-type? t ctxt)
  (match t
    [(type-var n) (unknown-type? (lookup-tvar ctxt n) ctxt)]
    [(type-unknown) #t]
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
(define (no-vars tu)
   (cond [(and (cons? tu) (type? (cdr tu))) tu]
         [(or (false? tu) (type? tu) (dim? tu)) (cons '() tu)]
         [else (error 'cry/unify "weird-result: ~a" tu)]))
(define (add-type-var name tu ctxt)
  (cond [(and (cons? tu) (or (dim? (cdr tu))
                             (type? (cdr tu))))
         (make-unified-result
          (cons (env-var name (unified-type tu)) (unified-vars tu))
          (unified-type tu))]
        [(or (type? tu) (dim? tu)) (cons (list (env-var name tu)) tu)]
        [(false? tu) (no-vars tu)]
        [else (error 'cry/unify "weird-result: ~a ~a" name tu)]))
;;  takes poly-type and maybe a concrete-type, tries to unify and also returns variable values figured
;; -> (cons new-type env-vars)
(trace-define
 (unify-type t1 t2 ctxt)
 (define (unk? t)
   (or (false? t) (type-unknown? t)))
 (define (unknown-type? t (ctxt ctxt))
   (match t
     [(type-var n) (unknown-type? (lookup-tvar ctxt n) ctxt)]
     [(type-unknown) #t]
     ;; [(type-integer) #t]
     [#f #t]
     [else #f]))


 (match* (t1 t2)
   [((type-var v1) (type-var v2))
    (define ut (unify-type (type-from-name ctxt v1) (type-from-name ctxt v2) ctxt))
    (if (cdr ut)
        (add-type-var v1 (add-type-var v2 ut ctxt) ctxt)
        (cons '() #f))]
   [((type-var n1) t2)
    #:when (not (unknown-type? t2))
    (match (type-from-name ctxt n1)
      [#f (if t2 (add-type-var n1 t2 ctxt) (cons '() #f))]
      [(type-integer) (add-type-var n1 (unify-type (type-sequence #f (type-bit)) t2 ctxt) ctxt)]
      [nt1 (add-type-var n1 (unify-type nt1 t2 ctxt) ctxt)])]
   [(t1 (type-var n2))
    #:when (not (unknown-type? t1))
    (unify-type t2 t1 ctxt)
    #;(match (type-from-name ctxt n2)
      [#f (if t1 (add-type-var n2 t1 ctxt) t1)]
      [nt2 (unify-type t1 nt2 ctxt)])]
   [((? unk?) t2) (no-vars t2)]
   [(t1 (? unk?)) (no-vars t1)]
   [((type-bit) _)  (no-vars t1)]
   [((type-integer) (type-integer)) (no-vars t1)]
   [((type-integer) (type-sequence d (type-bit))) (no-vars (type-sequence (or (maybe-full-type d ctxt) d) (type-bit)))]
   [((type-integer) (type-sequence d v))
    (apply-uts make-type-sequence (unify-type #f d ctxt) (unify-type (type-bit) v ctxt))
    ;; #:when (unknown-type? v) (no-vars (type-sequence d (type-bit)))
    ]
   [((type-sequence d v) (type-integer)) (unify-type t2 t1 ctxt)]
   [((type-tuple t1s ...) (type-tuple t2s ...))
    (define t12u (map (curryr unify-type ctxt) t1s t2s))
    (cons (append-map car t12u)
          (make-type-tuple (map cdr t12u)))
    ;; (define t12u (map (curryr unify-type ctxt) t1s t2s))
    ;; (cons (append-map car t12u) (make-type-tuple (map cdr t12u)))
    ]
   [((type-tuple t1s ...) (? unknown-type?)) (no-vars t1)]
   [((type-poly (v1ars ...) pt1) t2)
    (match-define (cons ut-vars utype) (unify-type pt1 t2 (update-env ctxt #:tvar (for/list ([v v1ars]) (env-var v #f)))))
    ;; TODO
    (cons (filter-not (λ (v) (member (env-var-name v) v1ars)) ut-vars) ;; TODO remove only first
          utype)]
   [((type-constraint (cs ...) ct1) t2) (unify-type ct1 t2 ctxt)]
   [((type-func ft1 tt1) (type-func ft2 tt2))
    (apply-uts make-type-func (unify-type ft1 ft2 ctxt) (unify-type ft1 ft2 ctxt))]
   ;; [((type-func ft1 ft2) t2) (unify-type t2 t1 ctxt)]
   [((type-sequence d1 t1) (type-sequence d2 t2))
    (apply-uts make-type-sequence (unify-type d1 d2 ctxt) (unify-type t1 t2 ctxt))]
   [((dim-var n1) (dim-int i)) (add-type-var n1 (unify-type t2 (type-from-name ctxt n1) ctxt) ctxt)]
   [((dim-var n1) t2)
    ;; (printf "lt: ~a\n" (type-from-name ctxt n1))
    (cond [(type-from-name ctxt n1) => (λ (t) (add-type-var n1 (unify-type t t2 ctxt) ctxt))]
          [(equal? t1 t2) (no-vars t1)]
          [else (unify-type (calc-dim t1 ctxt) (calc-dim t2 ctxt) ctxt) ;; (TODO "dim-var: ~a ~a\n" n1 t2)
           ])]
   [((dim-int i) (dim-var n2)) (add-type-var n2 (unify-type t1 (type-from-name ctxt n2) ctxt) ctxt)]
   [((dim-int i1) (dim-int i2))
    #:when (equal? i1 i2)
    (no-vars t1)]
   [((? dim-app?) t2) (unify-type (calc-dim t1 ctxt) t2 ctxt)]
   [(t1 (? dim-app?)) (unify-type (calc-dim t2 ctxt) t1 ctxt)]
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
    [(dim-var name) #f]
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
(trace-define (maybe-calc-type ast maybe-type^ ctxt)
  ;; (debug (printf "maybe-calc-type: ~a ~a\n" (pretty-cry ast) (pretty-cry maybe-type^)))
  ;; (define maybe-utype (unify-type (type-from-md ast) maybe-type^ ctxt))
  (define maybe-type maybe-type^ ;; (cdr maybe-utype)
    )
  (define (calc-type)
    (match ast
      [(expr-bind () body) (maybe-calc-type body maybe-type ctxt)]
      [(expr-where body) (maybe-calc-type body maybe-type ctxt)]
      [(expr-bind (ps ...) body) TODO]
      [(expr-where body ds ...) TODO]
      [(expr-app rator (pargs ...) vargs ...)
       (define rator-type (lookup-typeof ctxt (expr-var-name rator)))
       ;; (printf "calc-app-type: ~a ~a\n" rator (pretty-cry rator-type))
       (define-values (pvars new-type) (specialize-poly-type rator-type pargs vargs (update-context! ctxt #:type maybe-type)))
       (unify-type (last (get-farg-types new-type)) maybe-type ctxt)]
      [(expr-cond [chk thn] ... els) TODO]
      [(expr-var name)
       (define type-from-ctxt (lookup-typeof ctxt name))
       (define (unified-with-given)
         (if (and type-from-ctxt maybe-type)
             (unify-type type-from-ctxt maybe-type ctxt)
             (cons '() #f)))
       (define (type-from-ast prev-type)
         (match (lookup-env-vars (env-val (cc-env ctxt)) name)
           [(cons (env-lazy-var name f ast) rst) (maybe-calc-type ast prev-type ctxt)]
           [else (cons '() #f)]))
       ;; tries to figure out a type in order of, given type, unifying given with the one from syntax, calculating type from ast
       (cond
         [(maybe-full-type type-from-ctxt ctxt) => (λ (t) (cons '() t))]
         [else
          (let ([ut (unified-with-given)])
            (add-type-var name
                          (cond
                            [(maybe-full-type (cdr ut) ctxt) => (λ (t) (cons (car ut) t))]
                            [else
                             (let ([tfa (type-from-ast ut)])
                               (cond
                                 [(maybe-full-type (cdr tfa) ctxt) => (λ (t) (cons (append (car ut) (car tfa)) t))]
                                 [else (cons (append (car ut) (car tfa)) (cdr tfa))]))])
                          ctxt))])]
      [(expr-tvar name) (unify-type (type-from-name ctxt name) maybe-type ctxt)]
      [(expr-annot e t)
       (define ut (unify-type t maybe-type ctxt))
       (match-define (cons evs ct) (maybe-calc-type e (cdr ut) ctxt))
       (cons (append (car ut) evs) ct)]
      [(expr-lit i) (unify-type (type-integer) maybe-type ctxt)]
      [(expr-char c) (unify-type (type-sequence (dim-int 8) (type-bit)) maybe-type ctxt)]
      [(expr-tuple vs ...)
       (define vtus
         (map (λ (v t) (maybe-calc-type v t ctxt))
              vs
              (build-list (length vs) (λ (i) (maybe-tuple-type-i maybe-type i)))))
       ;; (debug (printf "throwing-away: ~a\n" (map car vtus)))
       (define ut (unify-type (make-type-tuple (map cdr vtus)) maybe-type ctxt))
       (make-unified-result (append (apply append (map unified-vars vtus)) (unified-vars ut)) (unified-type ut))]
      ;; [(expr-zero) (unify-type (type-integer) maybe-type ctxt)]

      [(expr-sequence-basic es ...)
       (define-values (elem-type evs)
         (for/fold [(t (maybe-sequence-elem-type maybe-type))
                    (evs '())]
                   [(e es)]
           (match-define (cons vs nt) (maybe-calc-type e t ctxt))
           (values nt (append vs evs))))
       (unless (empty? evs) (printf "TODO: throwing away: ~a\n" evs))
       (define sdim (dim-int (length es)))
       (unify-type (type-sequence sdim elem-type) maybe-type ctxt)]
      [(expr-sequence-enum from step to)
       (define sdim
         (if (and (expr-lit? from) (expr-lit? step) (expr-lit? to))
             (dim-int (/ (add1 (- (expr-lit-v to) (expr-lit-v from))) (expr-lit-v step)))
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
                                                 (env-var (pat-var-name vn) (maybe-sequence-elem-type vt))))))
       (apply-uts make-type-sequence maybe-dim-type maybe-elem-type)]))
  (update-type-md! ast ;; (if (concrete-type? maybe-type ctxt) maybe-utype (calc-type)) ;; TODO do deep poly check for concrete-type?
                   (calc-type)
                   ))

(define (kind-from-constraint name cs) (type-integer)) ;; TODO
(define (calc-dim-app da ctxt)
  (match-define (dim-app rator rands ...) da)
  (define rand-vals (map (compose maybe-dim-i (curryr calc-dim ctxt)) rands))
  ;; (debug (printf "calc-dim-app: ~a ~a\n" (pretty-cry da) rand-vals))
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
  ;; (debug (printf "calc-dim: ~a\n" (pretty-cry d)))
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

(trace-define (specialize-poly-type orig-type poly-args value-args ctxt)
  (define-values (uptype pvars cs) (unwrap-poly orig-type))
  (match-define (list varg-types^ ... res-type) (get-farg-types uptype))
  (define given-result-type (cc-type ctxt))
  (define pevs (given-pvars pvars poly-args cs))
  (define pctxt (update-env ctxt #:tvar pevs))

  ;; (debug (printf "specialize-poly-type: ~a\npargs: ~a\nvargs: ~a\n"
  ;;                (pretty-cry orig-type) (map pretty-cry poly-args) (map pretty-cry value-args))
  ;;        (printf "\tresult-type: ~a\n\tpevs: ~a\n" given-result-type pevs))
  (define res-ut-type (unify-type res-type given-result-type pctxt))
  ;; (debug (printf "spt: res-type:~a\n" (pretty-cry res-ut-type)))
  (define-values (varg-ctxt varg-types)
    (for/fold ([ctxt (update-env pctxt #:tvar (car res-ut-type))]
               [vts '()])
              ([v value-args]
               [t varg-types^])
      (define v-ut (maybe-calc-type v t (update-context! ctxt #:type t)))
      (define t-ut (unify-type (cdr v-ut) t ctxt))
      (values (update-env ctxt #:tvar (append (car v-ut) (car t-ut)))
              (cons (cdr t-ut) vts))))
  (define pargs-vals (for/list ([p pevs]) (lookup-tvar varg-ctxt (env-var-name p))))
  (printf "pargs-vals: ~a\n" pargs-vals)
  ;; (when (ormap false? pargs-vals)
  ;;   (print-cc varg-ctxt)
  ;;   (error 'sham/cry "cannot specialize: ~a ~a ~a\n" (pretty-cry orig-type) pargs-vals varg-types))
  (define full-func-type (maybe-full-type uptype varg-ctxt))
  (define ufunc-type (foldr make-type-func (cdr res-ut-type) (reverse varg-types)))
  (values (map (λ (p pv) (env-var (env-var-name p) pv)) pevs pargs-vals)
          (if full-func-type ;; (ormap false? pargs-vals)
              full-func-type
              (make-type-poly (filter (λ (v) (and (false? (cdr v))
                                                  (car v)))
                                      (map cons pevs pargs-vals))
                              ufunc-type)))

  #;(let* ([maybe-result-type (cc-type ctxt)]
         [maybe-varg-types (map (curryr maybe-calc-type ctxt) value-args (drop-right (get-farg-types uptype) 1))]
         [pvar-args (figure-out-pargs orig-type poly-args maybe-varg-types maybe-result-type ctxt)]
         [new-type (specialize-func-type uptype pvar-args maybe-varg-types maybe-result-type ctxt)])
    (debug (printf "specialized-poly-type: \n ~a\n ~a\n" orig-type new-type))
    (values pvar-args new-type)))
