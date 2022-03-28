#lang racket
(require (prefix-in cry-ir- "ast.rkt")
         "../ast.rkt"
         "../utils.rkt"
         "ctxt.rkt")
(require racket/trace)
(require sham/sam/pretty)
(provide calc-type
         ctype
         ;; unify-type
         specialize-poly-type
         )
(provide (all-defined-out))

;; (define (prettyify val)
;;   (match val
;;     [(? struct-cry-ast?) (pretty-cry val)]
;;     [else val]))

;; (define (trace-print-args fname vals kws kw-vals depth)
;;   (printf "~a> ~a: " (build-string depth (const #\ )) fname)
;;   (match fname
;;     [(or 'maybe-calc-type 'unify-type) (printf "~a - ~a\n" (pretty-cry (first vals)) (pretty-cry (second vals)))]
;;     ['specialize-poly-type (printf "~a:~a ~a -> ~a\n"
;;                                    (pretty-cry (first vals))
;;                                    (second vals)
;;                                    (map pretty-cry (third vals))
;;                                    (pretty-cry (fourth vals)))]
;;     [else (pretty-print (map prettyify vals))]))

;; (define (trace-print-results fname results depth)
;;   (printf "~a< ~a: " (build-string depth (const #\ )) fname)
;;   (define (type-vars vs)
;;     (for/list ([v vs])
;;       (printf " ~a:~a " (env-var-name v) (pretty-cry (env-var-val v)))))
;;   (match fname
;;     [(or 'unify-type 'maybe-calc-type) (printf "~a $" (pretty-cry (cdr (first results))))
;;                                        (type-vars (car (first results))) (newline)]
;;     ['specialize-poly-type (printf "~a :" (pretty-cry (second results)))
;;                            (type-vars (first results)) (newline)]
;;     [else (pretty-print (map prettyify results))]))

;; (current-trace-print-args trace-print-args)
;; (current-trace-print-results trace-print-results)

(define (ctype type ctxt)
  (printf "ctype: ~a\n" (pretty-cry type))
  (match type
    [(type-bit) (cry-ir-type-bit)]
    [(type-integer) (cry-ir-type-int)]
    [(type-sequence dim t) (cry-ir-type-seq (cdim dim ctxt) (ctype t ctxt))]
    [(type-tuple ts ...) (cry-ir-make-type-tup (map (curryr ctype ctxt) ts))]
    [(type-func from to) (match-define (list args ... ret) (get-farg-types type))
                         (cry-ir-make-type-fun '() (map (curryr ctype ctxt) args) (ctype ret ctxt))]
    [(type-var name) (define ct (ctxt-lookup-type-var ctxt name))
                     (printf "ct: ~a ~a\n" name ct)
                     ct]
    [(type-tdim d) (cdim d ctxt)]
    [(? type-poly?) (error 'cry/ir "poly type should be specialized at this point ~a" type)]
    [(type-constraint (cs ...) t) (ctype t ctxt)]))

;; -> (values spec-type poly-args)
(define (specialize-poly-type orig-type given-type-args given-val-args given-ret-type ctxt)
  (printf "\n\nspecialize: ~a ~a ~a ~a\n" (pretty-cry orig-type) (map pretty-cry given-type-args) (map pretty-cry given-val-args) (pretty-cry given-ret-type))
  (pretty-ctxt ctxt)
  (define-values (uptype pvars cs) (unwrap-poly orig-type))
  (match-define (list varg-types^ ... ret-type) (get-farg-types uptype))
  (define cgtas (map (curryr ctype ctxt) given-type-args))
  (define poly-ctxt
    (for/fold ([c ctxt])
              ([pvar pvars]
               [i (length pvars)])
      (if (< i (length given-type-args))
          (update-ctxt c #:types (env-var pvar (list-ref cgtas i)))
          (begin (printf "warn: poly type argument not given for type: ~a, pos: ~a\n" (pretty-cry orig-type) i) c))))
  (define-values (ret-ut-type ret-poly-vars) (unify-type ret-type given-ret-type poly-ctxt))
  (define-values (varg-ctxt varg-types)
    (for/fold ([varg-ctxt (update-ctxt poly-ctxt #:types ret-poly-vars)]
               [vts '()]
               #:result (values varg-ctxt (reverse vts)))
              ([v given-val-args]
               [t varg-types^])
      (define val-ir-type (ctype t varg-ctxt))
      (define-values (v-ut v-pvs) (calc-type v val-ir-type varg-ctxt))
      (define-values (t-ut t-pvs) (unify-type t v-ut (update-ctxt varg-ctxt #:types v-pvs)))
      (printf "varg: ~a ~a ~a\n" (pretty-cry t) t-ut (list v-pvs t-pvs))
      (values (update-ctxt varg-ctxt #:types (append v-pvs t-pvs))
              (cons t-ut vts))))
  (define-values (fin-ret-type _) (unify-type ret-type ret-ut-type varg-ctxt))
  (define parg-vals (for/list ([p pvars]) (ctxt-lookup-type-var varg-ctxt p)))
  (unless (andmap identity parg-vals) (error 'cry/ir "couldn't specialize type fully: ~a ~a\n" (pretty-cry orig-type) (map pretty-cry parg-vals)))
  ;; (define parg-evs (for/list ([n pvars] [v parg-vals]) (env-var n v)))
  (define fun-type (cry-ir-make-type-fun '() varg-types fin-ret-type))
  (printf "specialized ~a :> \n" (pretty-cry orig-type)) (pretty-print-ast fun-type) (newline)
  (values fun-type (map env-var pvars parg-vals)))

(define (full-ir-type cry-ir-type ctxt)
  (let rec ([cit cry-ir-type])
    (match cit
      [(cry-ir-type-bit) cit]
      [(cry-ir-type-int) cit]
      [(cry-ir-type-idx) cit]
      [(cry-ir-type-tup ts ...)
       (define cts (map rec ts))
       (and (andmap identity cts)
            (cry-ir-make-type-tup cts))]
      [(cry-ir-type-seq dim t)
       (define t^ (rec t))
       (and dim t^ (cry-ir-type-seq dim t^))]
      [(cry-ir-type-fun (pargs ...) (vargs ...) t)
       cit]
      [else #f])))
;; -> (values ir-type type-vars-vals)
(define (calc-type c-ast c-ir-type ctxt)
  (define fit (full-ir-type c-ir-type ctxt))
  (printf "calc-type: ~a\n" (pretty-cry c-ast)) (and c-ir-type (pretty-print-ast c-ir-type)) (newline)
  ;; (define (rec a i (c ctxt)) (calc-type a i c))
  ;; (define (udpate-ctxt-from-pats ps) c)
  (define (update-ctxt-with-defs ctxt ds)
    (for/fold ([c ctxt])
              ([d ds])
      (match d
        [(def-typed-val n t v)
         (update-ctxt ctxt #:typeofs (env-var n (calc-type v (ctype t c) c)))]
        [(def-combined (td ...) (to ...) (tvd ...) (ts ...))
         (update-ctxt-with-defs (update-ctxt-with-defs ctxt tvd) to)]
        [(def-typeof n t)
         (update-ctxt ctxt #:typeofs (env-var n (ctype t c)))])))
  (define (rec a it ctxt)
    (define (rec-list cs is ctxt)
      (for/fold ([ts '()]
                 [pevs '()]
                 #:result (values (reverse ts) (reverse pevs)))
                ([c cs]
                 [i is])
        (define-values (t evs) (calc-type c i (update-ctxt ctxt #:types pevs)))
      (values (cons t ts) (append evs pevs))))
    (printf "calc-type-rec: ~a\n" a) (and it (pretty-print-ast it)) (newline)
    (match a
      [(expr-bind () body)
       ;; (error 'stop)
       (printf "body: ~a\n" (pretty-cry body))
       (calc-type body it ctxt)]
      [(expr-where body) (calc-type body it ctxt)]
      ;; [(expr-bind (ps ...) body) (calc body it (update-ctxt-from-pats ps))]
      [(expr-where body ds ...) (calc-type body it (update-ctxt-with-defs ctxt ds))]
      [(expr-app rator (pargs ...) vargs ...)
       (define-values (_ rator-type) (ctxt-lookup-orig ctxt (expr-var-name rator)))
       (printf "calc-app-type: ~a ~a\n" rator (pretty-cry rator-type))
       (define-values (new-type pvars) (specialize-poly-type rator-type pargs vargs it ctxt))
       ;; todo calc types for vargs to get type var vals
       (values (cry-ir-type-fun-to new-type) '())]
      [(expr-cond [chk thn] ... els) (error 'cry/ir/type/todo)]
      [(expr-var name)
       (cond
         [(ctxt-has-val? ctxt name)
          (define var-val (ctxt-lookup-val ctxt name))
          (define val-type (cry-ir-def-val-type var-val))
          (values (combine-ir-type val-type it) '())]
         ;; [(ctxt-lookup-typeof ctxt name) => (λ (t) (values (combine-ir-type t it) '()))]
         [else
          (define-values (val type) (ctxt-lookup-orig ctxt name))
          (values (combine-ir-type (ctype type ctxt) it) '())])

       ;; (define (unified-with-given)
       ;;   (if (and type-from-ctxt it)
       ;;       (unify-type type-from-ctxt it ctxt)
       ;;       it))
       ;; (define (type-from-ast prev-type)
       ;;   (match (lookup-env-vars (env-val (cc-env ctxt)) name)
       ;;     [(cons (env-lazy-var name f ast) rst) (maybe-calc-type ast prev-type ctxt)]
       ;;     [else (cons '() #f)]))
       ;; tries to figure out a type in order of, given type, unifying given with the one from syntax, calculating type from ast
       ;; (cond
       ;;   [(maybe-full-type type-from-ctxt ctxt) => (λ (t) (cons '() t))]
       ;;   [else
       ;;    (let ([ut (unified-with-given)])
       ;;      (add-type-var name
       ;;                    (cond
       ;;                      [(maybe-full-type (cdr ut) ctxt) => (λ (t) (cons (car ut) t))]
       ;;                      [else
       ;;                       (let ([tfa (type-from-ast ut)])
       ;;                         (cond
       ;;                           [(maybe-full-type (cdr tfa) ctxt) => (λ (t) (cons (append (car ut) (car tfa)) t))]
       ;;                           [else (cons (append (car ut) (car tfa)) (cdr tfa))]))])
       ;;                    ctxt))])
       ]
      [(expr-tvar name) (values (ctxt-lookup-type-var ctxt name) '())]
      [(expr-annot e t)
       (define-values (t pvs) (unify-type t it ctxt))
       (calc-type e t (update-ctxt ctxt #:types pvs))]
      [(expr-lit i) (unify-type (type-integer) it ctxt)]
      [(expr-char c) (unify-type (type-sequence (dim-int 8) (type-bit)) it ctxt)]
      [(expr-tuple vs ...)
       (define-values (vts vts-pvs)
         (rec-list vs (build-list (length vs) (λ (i) (maybe-tup-type-i it i))) ctxt))
       ;; (debug (printf "throwing-away: ~a\n" (map car vtus)))
       ;; (define-values (ut pvs) (unify-type (make-type-tuple (map cdr vtus)) it ctxt))
       (values (cry-ir-make-type-tup vts) vts-pvs)
       ;; (make-unified-result (append (apply append (map unified-vars vtus)) (unified-vars ut)) (unified-type ut))
       ]
      ;; [(expr-zero) (unify-type (type-integer) maybe-type ctxt)]

      [(expr-sequence-basic es ...)
       (define-values (elem-type pevs)
         (for/fold [(t (maybe-seq-type-elem it))
                    (evs '())]
                   [(e es)]
           (define-values (et vs) (calc-type e t (update-ctxt ctxt #:types evs)))
           (values et (append vs evs))))
       (define-values (sdim dim-pevs) (unify-dim (dim-int (length es)) (maybe-seq-type-dim it) (update-ctxt ctxt #:types pevs)))
       (values (cry-ir-type-seq sdim elem-type) (append dim-pevs pevs))]
      [(expr-sequence-enum from step to)
       (define sdim
         (if (and (expr-lit? from) (expr-lit? step) (expr-lit? to))
             (dim-int (/ (add1 (- (expr-lit-v to) (expr-lit-v from))) (expr-lit-v step)))
             #f))
       (define-values (et etpvs) (unify-type (type-integer) (maybe-seq-type-elem it) ctxt))
       (define-values (dim dim-pevs) (unify-dim sdim (maybe-seq-type-dim it) ctxt))
       (values (cry-ir-type-seq dim et) (append dim-pevs etpvs))]
      [(expr-sequence-str s)
       (unify-type (type-sequence (dim-int (string-length s))
                                  (type-sequence (dim-int 8) (type-bit)))
                   it ctxt)]
      [(expr-sequence-comp body [(var val)] ...)
       (define-values (valts val-evs) (rec-list val (map (const #f) val) ctxt))

       ;; (define-values (val-tvars valts) (apply combine-unify-results (map (curryr maybe-calc-type #f ctxt) val)))
       (define val-lengths (map (compose maybe-seq-type-dim) valts))
       (define given-length (maybe-seq-type-dim it))
       (unless (or given-length (andmap identity val-lengths))
         (error 'sham/cryptol "unknown length for sequence comprehension ~a ~a" a val-lengths))
       (define-values (dim dim-pevs) (unify-dim (dim-int (apply min val-lengths)) given-length ctxt))
       ;; (define maybe-dim-type (cons '() dim))
       (define-values (elem-type elem-pevs)
         (calc-type body
              (maybe-seq-type-elem it)
              (update-ctxt ctxt
                           #:types (append dim-pevs val-evs)
                           #:typeofs (for/list ([vn var] [vt valts])
                                       (env-var (pat-var-name vn) (maybe-sequence-elem-type vt))))))
       (values (cry-ir-type-seq dim elem-type) (append dim-pevs elem-pevs val-evs))]))
  (if fit (values fit '()) (rec c-ast c-ir-type ctxt)))

;;  -> (values ir-type type-env-vars)
(define (unify-type cry-type ir-type ctxt)
  (printf "unifing: ~a \n" (pretty-cry cry-type)) (pretty-print-ast ir-type) (newline)
  (define (rec ct it) (unify-type ct it ctxt))
  (define (rec-list cts its)
    (for/fold ([ts '()]
               [pevs '()]
               #:result (values (reverse ts) (reverse pevs)))
              ([ct cts]
               [it its])
      (define-values (t evs) (unify-type ct it (update-ctxt ctxt #:types pevs)))
      (values (cons t ts) (append evs pevs))))
  (match* (cry-type ir-type)
    [(#f it) (values it '())]
    [(ct #f) (values (ctype ct ctxt) '())]
    [((type-var v) it)
     (define ct (ctxt-lookup-type-var ctxt v))
     (define cit (if ct (combine-ir-type ct it) it))
     (values cit (list (env-var v cit)))]
    [((type-bit) it) (values (combine-ir-type (cry-ir-type-bit) it) '())]
    ;; [((type-integer) (cry-ir-type-seq d e)) (cry-ir-type-int)]
    [((type-integer) it) (values (combine-ir-type (cry-ir-type-int) it) '())]
    [((type-sequence d e) (cry-ir-type-seq ir-d ir-e))
     (define-values (u-dim dim-pevs) (unify-dim d ir-d ctxt))
     (define-values (u-elem elem-pevs) (unify-type e ir-e (update-ctxt ctxt #:types dim-pevs)))
     (values (cry-ir-type-seq u-dim u-elem) (append dim-pevs elem-pevs))]
    [((type-sequence d e) (cry-ir-type-int))
     (values (cry-ir-type-seq (cdim d ctxt) (ctype e ctxt)) '())]
    [((type-tuple cts ...) (cry-ir-type-tup its ...))
     (define-values (ets e-pevs) (rec-list cts its))
     (values (cry-ir-make-type-tup ets) e-pevs)]
    [((type-poly (vars ...) pt) it)
     (unify-type pt it (update-ctxt ctxt #:types (map (curryr env-var #f) vars)))]
    [((type-func f1 t1) (cry-ir-type-fun (pvs ...) (frs ...) ts))
     (error 'cry/ir "unify func TODO")]
    [(_ _) (error 'cry/ir "cannot unify: ~a ~a" cry-type ir-type)]))
(define (combine-ir-type irt1 irt2)
  (match* (irt1 irt2)
    [(it1 it2) #:when (equal? it1 it2) it1]
    [(#f #f) #f]
    [(#f t2) t2]
    [(t1 #f) t1]
    [((cry-ir-type-bit) (cry-ir-type-bit)) irt1]
    [((cry-ir-type-int) (cry-ir-type-int)) irt1]
    [((cry-ir-type-idx) (cry-ir-type-idx)) irt1]
    [((cry-ir-type-tup t1s ...) (cry-ir-type-tup t2s ...))
     (cry-ir-make-type-tup (map combine-ir-type t1s t2s))]
    [((cry-ir-type-seq d1 e1) (cry-ir-type-seq d2 e2))
     (cry-ir-make-type-seq (or d1 d2) (combine-ir-type e1 e2))]
    [((cry-ir-type-fun (p1s ...) (v1s ...) r1)
      (cry-ir-type-fun (p2s ...) (v2s ...) r2))
     (cry-ir-make-type-fun (map combine-ir-type p1s p2s)
                           (map combine-ir-type v1s v2s)
                           (map combine-ir-type r1 r2))]))

(define (unify-dim cry-dim ir-dim ctxt)
  (printf "unifying-dim: ~a ~a\n" (pretty-cry cry-dim) ir-dim)
  (match* (cry-dim ir-dim)
    [((dim-var n) id)
     #:when ir-dim
     (values ir-dim (list (env-var n ir-dim)))]
    [((dim-int ic) ii)
     #:when (equal? ic ii)
     (values ic '())]
    [(cd id) (values (or (cdim cry-dim ctxt) ir-dim) '())]))

(define (get-farg-types cry-type)
  (match cry-type
    [(type-func frm to) (cons frm (get-farg-types to))]
    ;; [(type-poly (vars ...) t) (get-farg-types t)]
    ;; [(type-constraint (cs ...) t) (get-farg-types t)]
    [else (list cry-type)]))

(define (unwrap-poly cry-type (vs '()) (cs '()))
  (match cry-type
    [(type-poly (vars ...) t) (unwrap-poly t (append vs vars) cs)]
    [(type-constraint (cns ...) t) (unwrap-poly t vs (append cs cns))]
    [else (values cry-type vs cs)]))

;; cry-ast maybe
(define (maybe-seq-type-dim ir-type)
  (if (cry-ir-type-seq? ir-type) (cry-ir-type-seq-dim ir-type) #f))
(define (maybe-seq-type-elem ir-type)
  (if (cry-ir-type-seq? ir-type) (cry-ir-type-seq-elem ir-type) #f))
(define (maybe-tup-type-i ir-type idx)
  (if (cry-ir-type-tup? ir-type) (list-ref (cry-ir-type-tup-ts ir-type) idx) #f))
;; original cry ast maybe
(define (maybe-sequence-elem-type seq-type)
  (define ret
    (match seq-type
      [(type-sequence dim t) t]
      [(type-poly (vs ...) t) (maybe-sequence-elem-type t)]
      [(type-constraint (cs ...) t) (maybe-sequence-elem-type t)]
      [else #f]))
  (unless ret (printf "maybe-seq-elem: #f <- ~a\n" (pretty-cry seq-type)))
  ret)
(define (maybe-sequence-dim seq-type)
  (match seq-type
    [(type-sequence dim t) dim]
    [(type-poly (vs ...) t) (maybe-sequence-dim t)]
    [(type-constraint (cs ...) t) (maybe-sequence-dim t)]
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
;; ;; returns type with substituted poly vars from ctxt, if contains a type-poly or unknown var then #f
;; (define (maybe-full-type cry-type ctxt)
;;   (let rec ([t cry-type])
;;     (match t
;;       [#f #f]
;;       [(type-poly (vars ...) t) #f]
;;       [(type-constraint (cs ...) t)
;;        (define t^ (rec t))
;;        (and t^ (make-type-constraint cs t^))]
;;       [(type-sequence dim t)
;;        (define dim^ (rec dim))
;;        (define t^ (rec t))
;;        (and dim^ t^ (type-sequence dim^ t^))]
;;       [(type-tuple ts ...)
;;        (define ts^ (map rec ts))
;;        (and (andmap identity ts^) (make-type-tuple ts^))]
;;       [(type-var name)
;;        (define t^ (lookup-tvar ctxt name))
;;        (rec t^)]
;;       [(type-func from to)
;;        (define from^ (rec from))
;;        (define to^ (rec to))
;;        (and from^ to^ (type-func from^ to^))]
;;       [(type-bit) t]
;;       [(type-integer) #f]

;;       [(dim-int i) t]
;;       [(dim-var name) (printf "dim-var: ~a ~a\n" name (lookup-tvar ctxt name)) (rec (lookup-tvar ctxt name))]
;;       [(dim-app rator rands ...)
;;        (define rands^ (map rec rands))
;;        (define st (and (andmap identity rands^) (make-dim-app rator rands^)))
;;        (or (calc-dim-app st ctxt) st)])))
;; (define (try-specialize-type cry-type ctxt)
;;   (let rec ([t cry-type])
;;     (match t
;;       [(type-poly (vars ...) t) (make-type-poly vars (rec t))]
;;       [(type-constraint (cs ...) t)
;;        (define t^ (rec t))
;;        (and t^ (make-type-constraint cs t^))]
;;       [(type-sequence dim t)
;;        (type-sequence (rec dim) (rec t))]
;;       [(type-tuple ts ...)
;;        (make-type-tuple (map rec ts))]
;;       [(type-var name)
;;        (rec (lookup-tvar ctxt name))]
;;       [(type-func from to)
;;        (type-func (rec from) (rec to))]
;;       [(type-bit) t]
;;       [(type-integer) t]

;;       [(dim-int i) t]
;;       [(dim-var name) (rec (lookup-tvar ctxt name))]
;;       [(dim-app rator rands ...)
;;        (define st (make-dim-app rator (map rec rands)))
;;        (or (calc-dim-app st ctxt) st)])))
;; (define (unknown-type? t ctxt)
;;   (match t
;;     [(type-var n) (unknown-type? (lookup-tvar ctxt n) ctxt)]
;;     [(type-unknown) #t]
;;     [#f #t]
;;     [else #f]))

;; (define (poly-type? t ctxt)
;;   (match t
;;     [(type-poly (vs ...) t) #t]
;;     [(type-constraint (cs ...) t) (poly-type? t ctxt)]
;;     [else #f]))
;; (define (concrete-type? t ctxt)
;;   (and (not (unknown-type? t ctxt)) (not (poly-type? t ctxt))))

;; (define (type-from-name ctxt n)
;;   (or (lookup-tvar ctxt n) (lookup-type ctxt n)))

;; (define (combine-unify-results . urs)
;;   (values (append-map car urs) (map cdr urs)))
;; (define (apply-uts f . args)
;;   (define evs (map car args))
;;   (define ts (map cdr args))
;;   (cons (apply append evs) (apply f ts)))

;; (define make-unified-result cons)
;; (define unified-vars car)
;; (define unified-type cdr)
;; (define (no-vars tu)
;;    (cond [(and (cons? tu) (type? (cdr tu))) tu]
;;          [(or (false? tu) (type? tu) (dim? tu)) (cons '() tu)]
;;          [else (error 'cry/unify "weird-result: ~a" tu)]))
;; (define (add-type-var name tu ctxt)
;;   (cond [(and (cons? tu) (or (dim? (cdr tu))
;;                              (type? (cdr tu))
;;                              (false? (cdr tu))))
;;          (make-unified-result
;;           (cons (env-var name (unified-type tu)) (unified-vars tu))
;;           (unified-type tu))]
;;         [(or (type? tu) (dim? tu)) (cons (list (env-var name tu)) tu)]
;;         [(false? tu) (no-vars tu)]
;;         [else (error 'cry/unify "weird-result: ~a ~a" name tu)]))
;; ;;  takes poly-type and maybe a concrete-type, tries to unify and also returns variable values figured
;; ;; -> (cons new-type env-vars)
;; (define
;;  (unify-type t1 t2 ctxt)
;;  (define (unk? t)
;;    (or (false? t) (type-unknown? t)))
;;  (define (unknown-type? t (ctxt ctxt))
;;    (match t
;;      [(type-var n) (unknown-type? (lookup-tvar ctxt n) ctxt)]
;;      [(type-unknown) #t]
;;      ;; [(type-integer) #t]
;;      [#f #t]
;;      [else #f]))


;;  (match* (t1 t2)
;;    [((type-var v1) (type-var v2))
;;     (define ut (unify-type (type-from-name ctxt v1) (type-from-name ctxt v2) ctxt))
;;     (if (cdr ut)
;;         (add-type-var v1 (add-type-var v2 ut ctxt) ctxt)
;;         (cons '() #f))]
;;    [((type-var n1) t2)
;;     #:when (not (unknown-type? t2))
;;     (match (type-from-name ctxt n1)
;;       [#f (if t2 (add-type-var n1 t2 ctxt) (cons '() #f))]
;;       [(type-integer) (add-type-var n1 (unify-type (type-sequence #f (type-bit)) t2 ctxt) ctxt)]
;;       [nt1 (add-type-var n1 (unify-type nt1 t2 ctxt) ctxt)])]
;;    [(t1 (type-var n2))
;;     #:when (not (unknown-type? t1))
;;     (unify-type t2 t1 ctxt)
;;     #;(match (type-from-name ctxt n2)
;;       [#f (if t1 (add-type-var n2 t1 ctxt) t1)]
;;       [nt2 (unify-type t1 nt2 ctxt)])]
;;    [((? unk?) t2) (no-vars t2)]
;;    [(t1 (? unk?)) (no-vars t1)]
;;    [((type-bit) _)  (no-vars t1)]
;;    [((type-integer) (type-integer)) (no-vars t1)]
;;    [((type-integer) (type-sequence d (type-bit))) (no-vars (type-sequence (or (maybe-full-type d ctxt) d) (type-bit)))]
;;    [((type-integer) (type-sequence d v))
;;     (apply-uts make-type-sequence (unify-type #f d ctxt) (unify-type (type-bit) v ctxt))
;;     ;; #:when (unknown-type? v) (no-vars (type-sequence d (type-bit)))
;;     ]
;;    [((type-sequence d v) (type-integer)) (unify-type t2 t1 ctxt)]
;;    [((type-tuple t1s ...) (type-tuple t2s ...))
;;     (define t12u (map (curryr unify-type ctxt) t1s t2s))
;;     (cons (append-map car t12u)
;;           (make-type-tuple (map cdr t12u)))
;;     ;; (define t12u (map (curryr unify-type ctxt) t1s t2s))
;;     ;; (cons (append-map car t12u) (make-type-tuple (map cdr t12u)))
;;     ]
;;    [((type-tuple t1s ...) (? unknown-type?)) (no-vars t1)]
;;    [((type-poly (v1ars ...) pt1) t2)
;;     (match-define (cons ut-vars utype) (unify-type pt1 t2 (update-env ctxt #:tvar (for/list ([v v1ars]) (env-var v #f)))))
;;     ;; TODO
;;     (cons (filter-not (λ (v) (member (env-var-name v) v1ars)) ut-vars) ;; TODO remove only first
;;           utype)]
;;    [((type-constraint (cs ...) ct1) t2) (unify-type ct1 t2 ctxt)]
;;    [((type-func ft1 tt1) (type-func ft2 tt2))
;;     (apply-uts make-type-func (unify-type ft1 ft2 ctxt) (unify-type ft1 ft2 ctxt))]
;;    ;; [((type-func ft1 ft2) t2) (unify-type t2 t1 ctxt)]
;;    [((type-sequence d1 t1) (type-sequence d2 t2))
;;     (apply-uts make-type-sequence (unify-type d1 d2 ctxt) (unify-type t1 t2 ctxt))]
;;    [((dim-var n1) (dim-int i)) (add-type-var n1 (unify-type t2 (type-from-name ctxt n1) ctxt) ctxt)]
;;    [((dim-var n1) t2)
;;     ;; (printf "lt: ~a\n" (type-from-name ctxt n1))
;;     (cond [(type-from-name ctxt n1) => (λ (t) (add-type-var n1 (unify-type t t2 ctxt) ctxt))]
;;           [(equal? t1 t2) (no-vars t1)]
;;           [else (unify-type (calc-dim t1 ctxt) (calc-dim t2 ctxt) ctxt) ;; (TODO "dim-var: ~a ~a\n" n1 t2)
;;            ])]
;;    [((dim-int i) (dim-var n2)) (add-type-var n2 (unify-type t1 (type-from-name ctxt n2) ctxt) ctxt)]
;;    [((dim-int i1) (dim-int i2))
;;     #:when (equal? i1 i2)
;;     (no-vars t1)]
;;    [((? dim-app?) (? dim-app?))
;;     (cond [(calc-dim t1 ctxt) => (λ (cd) (unify-type cd t2 ctxt))]
;;           [(calc-dim t2 ctxt) => (λ (cd) (unify-type t1 cd ctxt))]
;;           [else (error 'sham/cry/unify "couldn't unify dims: ~a ~a" t1 t2)])]
;;    [((? dim-app?) t2) (unify-type (calc-dim t1 ctxt) t2 ctxt)]
;;    [(t1 (? dim-app?)) (unify-type (calc-dim t2 ctxt) t1 ctxt)]
;;    [(t1 t2) #:when (equal? t1 t2) (no-vars t1)]
;;    ;; [(ut1 t2) #:when (unknown-type? ut1 ctxt) t2]
;;    ;; [(t1 ut2) #:when (unknown-type? ut2 ctxt) t1]
;;    [(_ _)
;;     (debug (print-cc ctxt))
;;     (error 'sham/cryptol/unify "mismatch types: \n~a \n~a" t1 t2)]))

(define (maybe-dim-i dim)
  (match dim
    [(dim-int i) i]
    [(dim-var name) #f]
    [else #f]))


;; ;; TODO cache unified/calculated types
;; (define (type-from-md ast) #f)
;; (define (update-type-md! ast type) type)

;; ;; returns similar result as unify-type: (cons vars type)
;; (define (maybe-calc-type ast maybe-type^ ctxt)
;;   ;; (debug (printf "maybe-calc-type: ~a ~a\n" (pretty-cry ast) (pretty-cry maybe-type^)))
;;   ;; (define maybe-utype (unify-type (type-from-md ast) maybe-type^ ctxt))
;;   (define maybe-type maybe-type^ ;; (cdr maybe-utype)
;;     )
;;   (define (calc-type)
;;     (match ast
;;       [(expr-bind () body) (maybe-calc-type body maybe-type ctxt)]
;;       [(expr-where body) (maybe-calc-type body maybe-type ctxt)]
;;       [(expr-bind (ps ...) body) TODO]
;;       [(expr-where body ds ...) TODO]
;;       [(expr-app rator (pargs ...) vargs ...)
;;        (define rator-type (lookup-typeof ctxt (expr-var-name rator)))
;;        ;; (printf "calc-app-type: ~a ~a\n" rator (pretty-cry rator-type))
;;        (define-values (pvars new-type) (specialize-poly-type rator-type pargs vargs (update-context! ctxt #:type maybe-type)))
;;        (unify-type (last (get-farg-types new-type)) maybe-type ctxt)]
;;       [(expr-cond [chk thn] ... els) TODO]
;;       [(expr-var name)
;;        (define type-from-ctxt (lookup-typeof ctxt name))
;;        (define (unified-with-given)
;;          (if (and type-from-ctxt maybe-type)
;;              (unify-type type-from-ctxt maybe-type ctxt)
;;              (cons '() #f)))
;;        (define (type-from-ast prev-type)
;;          (match (lookup-env-vars (env-val (cc-env ctxt)) name)
;;            [(cons (env-lazy-var name f ast) rst) (maybe-calc-type ast prev-type ctxt)]
;;            [else (cons '() #f)]))
;;        ;; tries to figure out a type in order of, given type, unifying given with the one from syntax, calculating type from ast
;;        (cond
;;          [(maybe-full-type type-from-ctxt ctxt) => (λ (t) (cons '() t))]
;;          [else
;;           (let ([ut (unified-with-given)])
;;             (add-type-var name
;;                           (cond
;;                             [(maybe-full-type (cdr ut) ctxt) => (λ (t) (cons (car ut) t))]
;;                             [else
;;                              (let ([tfa (type-from-ast ut)])
;;                                (cond
;;                                  [(maybe-full-type (cdr tfa) ctxt) => (λ (t) (cons (append (car ut) (car tfa)) t))]
;;                                  [else (cons (append (car ut) (car tfa)) (cdr tfa))]))])
;;                           ctxt))])]
;;       [(expr-tvar name) (unify-type (type-from-name ctxt name) maybe-type ctxt)]
;;       [(expr-annot e t)
;;        (define ut (unify-type t maybe-type ctxt))
;;        (match-define (cons evs ct) (maybe-calc-type e (cdr ut) ctxt))
;;        (cons (append (car ut) evs) ct)]
;;       [(expr-lit i) (unify-type (type-integer) maybe-type ctxt)]
;;       [(expr-char c) (unify-type (type-sequence (dim-int 8) (type-bit)) maybe-type ctxt)]
;;       [(expr-tuple vs ...)
;;        (define vtus
;;          (map (λ (v t) (maybe-calc-type v t ctxt))
;;               vs
;;               (build-list (length vs) (λ (i) (maybe-tuple-type-i maybe-type i)))))
;;        ;; (debug (printf "throwing-away: ~a\n" (map car vtus)))
;;        (define ut (unify-type (make-type-tuple (map cdr vtus)) maybe-type ctxt))
;;        (make-unified-result (append (apply append (map unified-vars vtus)) (unified-vars ut)) (unified-type ut))]
;;       ;; [(expr-zero) (unify-type (type-integer) maybe-type ctxt)]

;;       [(expr-sequence-basic es ...)
;;        (define-values (elem-type evs)
;;          (for/fold [(t (maybe-sequence-elem-type maybe-type))
;;                     (evs '())]
;;                    [(e es)]
;;            (match-define (cons vs nt) (maybe-calc-type e t ctxt))
;;            (values nt (append vs evs))))
;;        ;; (unless (empty? evs) (printf "TODO: throwing away: ~a\n" evs))
;;        (define sdim (dim-int (length es)))
;;        (define ut (unify-type (type-sequence sdim elem-type) maybe-type ctxt))
;;        (make-unified-result (append evs (unified-vars ut)) (unified-type ut))]
;;       [(expr-sequence-enum from step to)
;;        (define sdim
;;          (if (and (expr-lit? from) (expr-lit? step) (expr-lit? to))
;;              (dim-int (/ (add1 (- (expr-lit-v to) (expr-lit-v from))) (expr-lit-v step)))
;;              #f))
;;        (unify-type (type-sequence sdim (type-integer)) maybe-type ctxt)]
;;       [(expr-sequence-str s)
;;        (unify-type (type-sequence (dim-int (string-length s))
;;                                   (type-sequence (dim-int 8) (type-bit)))
;;                    maybe-type ctxt)]
;;       [(expr-sequence-comp body [(var val)] ...)
;;        (define-values (val-tvars valts) (apply combine-unify-results (map (curryr maybe-calc-type #f ctxt) val)))
;;        (define val-lengths (map (compose maybe-dim-i maybe-sequence-dim) valts))
;;        (define given-length (maybe-sequence-dim maybe-type))
;;        (unless (or given-length (andmap identity val-lengths))
;;          (error 'sham/cryptol "unknown length for sequence comprehension ~a ~a" ast val-lengths))
;;        (define dim (or given-length (dim-int (apply min val-lengths))))
;;        (define maybe-dim-type (cons '() dim))
;;        (define maybe-elem-type
;;          (maybe-calc-type body (maybe-sequence-elem-type maybe-type)
;;                           (update-env ctxt
;;                                       ;; #:type val-tvars
;;                                       #:typeof (for/list ([vn var] [vt valts])
;;                                                  (env-var (pat-var-name vn) (maybe-sequence-elem-type vt))))))
;;        (apply-uts make-type-sequence maybe-dim-type maybe-elem-type)]))
;;   (update-type-md! ast ;; (if (concrete-type? maybe-type ctxt) maybe-utype (calc-type)) ;; TODO do deep poly check for concrete-type?
;;                    (calc-type)
;;                    ))

;; (define (kind-from-constraint name cs) (type-integer)) ;; TODO
(define (cdim-app da ctxt)
  (match-define (dim-app rator rands ...) da)
  (define rand-vals (map (curryr cdim ctxt) rands))
  (printf "calc-dim-app: ~a ~a\n" (pretty-cry da) rand-vals)
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
  (if (ormap false? rand-vals) #f (apply f rand-vals)))

(define (cdim d ctxt)
  (printf "calc-dim: ~a\n" (pretty-cry d))
  (pretty-ctxt ctxt)
  (match d
    [(dim-int i) i]
    [(dim-var n) (ctxt-lookup-type-var ctxt n)]
    [(? dim-app?) (cdim-app d ctxt)]
    [#f #f]))

;; (define (contains-var? t)
;;   (match t
;;     [(type-var _) #t]
;;     [(type-sequence d t) (and (contains-var? d) (contains-var? t))]
;;     [(type-tuple ts ...) (andmap contains-var? ts)]
;;     [(type-bit) #f]
;;     [(type-integer) #f]

;;     [(dim-var _) #t]
;;     [(dim-int i) #f]))

;; ;; TODO kind of pvars
;; ;; (kind-from-constraint pvar cs)
;; ;; tries to match given poly type args with needed poly type vars
;; (define (given-pvars pvars pargs cs)
;;   (for/list ([pvar pvars]
;;              [i (length pvars)])
;;     (env-var pvar (if (< i (length pargs)) (list-ref pargs i) #f))))

;; (define (specialize-poly-type orig-type poly-args value-args ctxt)
;;   (define-values (uptype pvars cs) (unwrap-poly orig-type))
;;   (match-define (list varg-types^ ... res-type) (get-farg-types uptype))
;;   (define given-result-type (cc-type ctxt))
;;   (define pevs (given-pvars pvars poly-args cs))
;;   (define pctxt (update-env ctxt #:tvar pevs))

;;   (debug (printf "specialize-poly-type: ~a\npargs: ~a\nvargs: ~a\n"
;;                  (pretty-cry orig-type) (map pretty-cry poly-args) (map pretty-cry value-args))
;;          (printf "\tresult-type: ~a\n\tpevs: ~a\n" given-result-type pevs))
;;   (define res-ut-type (unify-type res-type given-result-type pctxt))
;;   (debug (printf "spt: res-type:~a\n" (pretty-cry res-ut-type)))
;;   (define-values (varg-ctxt varg-types)
;;     (for/fold ([ctxt (update-env pctxt #:tvar (car res-ut-type))]
;;                [vts '()])
;;               ([v value-args]
;;                [t varg-types^])
;;       (define v-ut (maybe-calc-type v t (update-context! ctxt #:type t)))
;;       (define t-ut (unify-type (cdr v-ut) t ctxt))
;;       (values (update-env ctxt #:tvar (append (car v-ut) (car t-ut)))
;;               (cons (cdr t-ut) vts))))
;;   (define pargs-vals (for/list ([p pevs]) (lookup-tvar varg-ctxt (env-var-name p))))
;;   (printf "pargs-vals: ~a\n" pargs-vals)
;;   ;; (when (ormap false? pargs-vals)
;;   ;;   (print-cc varg-ctxt)
;;   ;;   (error 'sham/cry "cannot specialize: ~a ~a ~a\n" (pretty-cry orig-type) pargs-vals varg-types))
;;   (define full-func-type (maybe-full-type uptype varg-ctxt))
;;   (printf "full-func-type: ~a\n" (pretty-cry full-func-type))
;;   (define ufunc-type (foldr make-type-func (cdr res-ut-type) (reverse varg-types)))
;;   (define final-type
;;     (if full-func-type ;; (ormap false? pargs-vals)
;;         full-func-type
;;         (make-type-poly (filter (λ (v) (and (false? (cdr v))
;;                                             (car v)))
;;                                 (map cons pevs pargs-vals))
;;                         ufunc-type)))
;;   (printf "specialize-poly-type: final-type: ~a\n" (pretty-cry final-type))
;;   (values (map (λ (p pv) (env-var (env-var-name p) pv)) pevs pargs-vals) final-type)

;;   #;(let* ([maybe-result-type (cc-type ctxt)]
;;          [maybe-varg-types (map (curryr maybe-calc-type ctxt) value-args (drop-right (get-farg-types uptype) 1))]
;;          [pvar-args (figure-out-pargs orig-type poly-args maybe-varg-types maybe-result-type ctxt)]
;;          [new-type (specialize-func-type uptype pvar-args maybe-varg-types maybe-result-type ctxt)])
;;     (debug (printf "specialized-poly-type: \n ~a\n ~a\n" orig-type new-type))
;;     (values pvar-args new-type)))
