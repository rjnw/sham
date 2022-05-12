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
  ;; (printf "ctype: ~a\n" (pretty-cry type))
  (match type
    [(type-bit) (cry-ir-type-bit)]
    [(type-integer) (cry-ir-type-int)]
    [(type-sequence dim t) (cry-ir-type-seq (cdim dim ctxt) (ctype t ctxt))]
    [(type-tuple ts ...) (cry-ir-make-type-tup (map (curryr ctype ctxt) ts))]
    [(type-func from to) (match-define (list args ... ret) (get-farg-types type))
                         (cry-ir-make-type-fun '() (map (curryr ctype ctxt) args) (ctype ret ctxt))]
    [(type-var name) (define ct (ctxt-lookup-type-var ctxt name))
                     ;; (printf "ct: ~a ~a\n" name ct)
                     ct]
    [(type-tdim d) (cdim d ctxt)]
    [(? type-poly?) (error 'cry/ir "poly type should be specialized at this point ~a" type)]
    [(type-constraint (cs ...) t) (ctype t ctxt)]))

;; -> (values spec-type poly-args)
(define (specialize-poly-type orig-type given-type-args given-val-args given-ret-type ctxt)
  ;; (printf "\n\nspecialize: ~a ~a ~a ~a\n" (pretty-cry orig-type) (map pretty-cry given-type-args) (map pretty-cry given-val-args) (pretty-cry given-ret-type))
  ;; (pretty-ctxt ctxt)
  (define-values (uptype pvars cs) (unwrap-poly orig-type))
  (match-define (list varg-types^ ... ret-type) (get-farg-types uptype))
  (define cgtas (map (curryr ctype ctxt) given-type-args))
  (define poly-ctxt
    (for/fold ([c ctxt])
              ([pvar pvars]
               [i (length pvars)])
      (if (< i (length given-type-args))
          (update-ctxt c #:types (list (env-var pvar (list-ref cgtas i))))
          (begin ;; (printf "warn: poly type argument not given for type: ~a, pos: ~a\n" (pretty-cry orig-type) i)
            c))))
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
      ;; (printf "varg: ~a ~a ~a\n" (pretty-cry t) t-ut (list v-pvs t-pvs))
      (values (update-ctxt varg-ctxt #:types (append v-pvs t-pvs))
              (cons t-ut vts))))
  (define-values (fin-ret-type _) (unify-type ret-type ret-ut-type varg-ctxt))
  (define parg-vals (for/list ([p pvars]) (ctxt-lookup-type-var varg-ctxt p)))
  (unless (andmap identity parg-vals) (error 'cry/ir "couldn't specialize type fully: ~a ~a\n" (pretty-cry orig-type) (map pretty-cry parg-vals)))
  ;; (define parg-evs (for/list ([n pvars] [v parg-vals]) (env-var n v)))
  (define fun-type (cry-ir-make-type-fun '() varg-types fin-ret-type))
  ;; (printf "specialized ~a :> \n" (pretty-cry orig-type)) (pretty-print-ast fun-type) (newline)
  (values fun-type (map env-var pvars parg-vals)))

(define (full-ir-type cry-ir-type ctxt)
  (unless (or (false? cry-ir-type) (cry-ir-type? cry-ir-type))
    (error 'cry/ir "type is not ir type: ~a" cry-ir-type))
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
  ;; (printf "calc-type: ~a\n" (pretty-cry c-ast)) (and c-ir-type (pretty-print-ast c-ir-type)) (when fit (printf " fit: ~a" fit)) (newline)
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
        (define-values (typ evs) (calc-type c i (update-ctxt ctxt #:types pevs)))
        (values (cons typ ts) (append evs pevs))))
    ;; (printf "calc-type-rec: ~a " (pretty-cry a)) (print a) (and it (pretty-print-ast it)) (newline)
    (match a
      [(expr-bind () body)
       ;; (error 'stop)
       ;; (printf "body: ~a\n" (pretty-cry body))
       (calc-type body it ctxt)]
      [(expr-where body) (calc-type body it ctxt)]
      ;; [(expr-bind (ps ...) body) (calc body it (update-ctxt-from-pats ps))]
      [(expr-where body ds ...) (calc-type body it (update-ctxt-with-defs ctxt ds))]
      [(expr-app rator (pargs ...) vargs ...)
       (define-values (_ rator-type) (ctxt-lookup-orig ctxt (expr-var-name rator)))
       ;; (printf "calc-app-type: ~a ~a ~a\n" rator (pretty-cry rator-type) (cry-ir-type? it))
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
          (values (combine-ir-type (ctype type ctxt) it) '())])]
      [(expr-tvar name) (values (ctxt-lookup-type-var ctxt name) '())]
      [(expr-annot e t)
       (define-values (typ pvs) (unify-type t it ctxt))
       (calc-type e typ (update-ctxt ctxt #:types pvs))]
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
  ;; (printf "unifing: ~a \n" (pretty-cry cry-type)) (pretty-print-ast ir-type) (newline)
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
                           (map combine-ir-type r1 r2))]
    [(it1 it2) (error 'cry/ir "cannot combine ~a ~a" (cry-ir-type-bit? irt1) (cry-ir-type-bit? irt2))]))

(define (unify-dim cry-dim ir-dim ctxt)
  ;; (printf "unifying-dim: ~a ~a\n" (pretty-cry cry-dim) ir-dim)
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
  ;; (unless ret (printf "maybe-seq-elem: #f <- ~a\n" (pretty-cry seq-type)))
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

(define (maybe-dim-i dim)
  (match dim
    [(dim-int i) i]
    [(dim-var name) #f]
    [else #f]))

(define (cdim-app da ctxt)
  (match-define (dim-app rator rands ...) da)
  (define rand-vals (map (curryr cdim ctxt) rands))
  ;; (printf "calc-dim-app: ~a ~a\n" (pretty-cry da) rand-vals)
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
  ;; (printf "calc-dim: ~a\n" (pretty-cry d))
  ;; (pretty-ctxt ctxt)
  (match d
    [(dim-int i) i]
    [(dim-var n) (ctxt-lookup-type-var ctxt n)]
    [(? dim-app?) (cdim-app d ctxt)]
    [#f #f]))
