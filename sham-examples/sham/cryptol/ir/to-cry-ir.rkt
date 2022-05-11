#lang racket

(require racket/syntax
         (for-syntax syntax/parse))

(require (prefix-in cry-ir- "ast.rkt")
         "../ast.rkt"
         "../utils.rkt"
         "ctxt.rkt"
         "type.rkt")

(provide to-cry-ir)
(define TODO #f)

;; result of a compile: [ir-expr bind-pairs specializations]
(struct res [vl bp sp] #:transparent)
(define (combine-res lres)
  (define-values
    (vals binds specs)
    (for/fold ([vals '()]
               [binds '()]
               [specs '()])
              ([r lres]
               #:when r)
      (match-define (res vl bs sp) r)
      (values (cons vl vals)
              (cons bs binds)
              (cons sp specs))))
  (res (reverse vals) (reverse binds) (reverse specs)))
(define (eres (e #f)) (if (res? e) e (res e '() '())))
(define (ures r #:val (v #f) #:bind (b #f) #:spec (s #f))
  (match-define (res vl bp sp) (or r (eres v)))
  (define (maybe-cons fst rst)
    (flatten
     (cond [(list? fst) (append fst rst)]
           [fst (cons fst rst)]
           [else rst])))
  (res (or v vl) (maybe-cons b bp) (maybe-cons s sp)))
(define (set-res-val r v) (ures r #:val v))
(define (add-spec r spec) (ures r #:spec spec))
(define (add-bind r bind) (ures r #:bind bind))
(define (with-e r) (values (res-vl r) r))
(define (combine-for-do body ress)
  (set-res-val (combine-res (list (combine-res ress) body))
               (res-vl body)))
(define-syntax (do-res stx)
  (syntax-parse stx
    [(_ [name:id vals:expr] ... body)
     #:with (name-res ...) (generate-temporaries #`(name ...))
     #`(let*-values ([(name name-res) (with-e vals)] ...)
         (combine-for-do (eres body) (list name-res ...)))]))

(define (cry-ir-dim-val n) (cry-ir-expr-lit (cry-ir-type-idx) n))

(define (cpat pat-ast ir-val)
  (define type (cry-ir-expr-type ir-val))
  (match pat-ast
    [(pat-var name) (list (cry-ir-def-val name type ir-val))]
    [(pat-tuple ps ...)
     (apply append
            (for/list ([p ps]
                       [i (length ps)]
                       [t (cry-ir-type-tup-ts type)])
              (cpat p (cry-ir-expr-tup-idx t ir-val (cry-ir-dim-val i)))))]
    [(pat-sequence ps ...)
     (define elem-typ (cry-ir-type-seq-elem type))
     (apply append
            (for/list ([p ps]
                       [i (length ps)])
              (cpat p (cry-ir-expr-seq-idx elem-typ ir-val (cry-ir-dim-val i)))))]))



(define (specialize-rator rator-name rator-val spec-type pargs ctxt)
  (define (cfdef)
    ;; (match-define (def-typed-val name type val) tv)
    (match-define (expr-bind (ps ...) body) rator-val)
    (match-define (cry-ir-type-fun (ptypes ...) (vtypes ...) rtype) spec-type)
    (unless (empty? ptypes) (error 'cry/ir "unsupported non specialized poly types ~a" ptypes))
    (define fargs (for/list ([t vtypes]
                             [i (build-list (length vtypes) identity)])
                    (cry-ir-expr-frg t i)))
    (define pat-binds (append-map cpat ps fargs))
    (match-define (res cbody bbinds bsp) (cexpr body rtype (update-ctxt ctxt #:vals pat-binds #:types pargs)))
    ;; (printf "pb: ~a, bb: ~a\n" pat-binds bbinds)
    (define cval (cry-ir-def-cfun rator-name spec-type (cry-ir-make-expr-bnd rtype (append pat-binds bbinds) cbody)))
    (res cval '()  bsp))  ;; compile a function for rator with specialized type and poly-args

  (define (cfprim)
    (res (cry-ir-make-def-pfun rator-name pargs spec-type) '() '()))
  (define cres
    (cond
      [(expr-bind? rator-val) (cfdef)]
      [(equal? rator-val 'primitive) (cfprim)]
      [else (error 'cry/ir "TODO rator ~a" rator-val)]))
  (match-define (res cval '() sps) cres)
  (define sval (cry-ir-expr-fvar spec-type cval))
  (define specs (cons cval sps))
  (values sval specs))

;; -> (values res/without-val ctxt)
(define (cdef d ctxt)
  ;; TODO other defs
  (match d
    [(def-typed-val name type val)
     (define ir-type (ctype type ctxt))
     (printf "\n\ndef-typed-val: ~a ~a ~a ~a\n" name (pretty-cry type) ir-type (pretty-cry val))
     (match-define (res vl bs sp) (cexpr val ir-type (update-ctxt ctxt #:vals (list (cry-ir-def-val name ir-type (cry-ir-expr-bnv ir-type name))))))
     (define val-def (cry-ir-def-val name ir-type vl))
     (values (res #f (append (list val-def) bs) sp) (update-ctxt ctxt #:vals (list val-def)))]
    [(def-combined (ts ...) (tos ...) (tvs ...) ())
     (define type-ctxt
       (update-ctxt ctxt
                    #:types (for/list ([t ts])
                              (match-define (def-type name val) t)
                              (env-var name (ctype val ctxt)))))
     (define typofs-ctxt
       (update-ctxt type-ctxt
                    #:typeofs (apply append
                                     (for/list ([to tos])
                                       (match-define (def-typeof ns ... v) to)
                                       (define t^ (ctype v type-ctxt))
                                       (map (λ (n) (env-var n t^)) ns)))))
     (define (do-val-ctxt ctxt)
       (update-ctxt ctxt
                    #:typeofs (for/list ([tv tvs])
                                (match-define (def-typed-val name type val) tv)
                                (env-var name (ctype type ctxt)))))

     (define-values (val-binds val-specs val-ctxt)
       (for/fold ([binds '()]
                  [specs '()]
                  [ctxt (do-val-ctxt (do-val-ctxt typofs-ctxt))]
                  #:result (values (reverse binds) specs ctxt))
                 ([tv tvs])
         (match-define (def-typed-val name type val) tv)
         (define ir-type (ctype type ctxt))
         (define ir-val-res (cexpr val ir-type (update-ctxt ctxt
                                                            #:vals (list (cry-ir-def-val name ir-type (cry-ir-expr-bnv ir-type name)))
                                                            #:typeofs (list (env-var name ir-type)))))
         ;; (error 'todo "cry-ir-def-val only needs ir-expr inside res (ir-val)")
         (match-define (res vl bs sp) ir-val-res)
         (define ir-def (cry-ir-def-val name ir-type vl))
         (values (append bs (cons ir-def binds)) (append sp specs) (update-ctxt ctxt #:vals (list ir-def)))))
     (values (res #f val-binds val-specs) val-ctxt)]))

;; (define (get-tvs d)
;;   (match d
;;     [(def-combined () () (tvs ...) ())
;;      tvs]))

;; -> res
(define (cexpr e maybe-given-type ctxt)
  (printf "\ncexpr: ~a : ~a\n" (pretty-cry e) maybe-given-type)
  ;; (pretty-ctxt ctxt)
  (define-values (type tvars) (calc-type e maybe-given-type ctxt))
  (printf "\tcexpr-type: ~a\n" type)
  (unless (empty? tvars) (printf "tvars: ~a\n" tvars))
  (define (do-one e t (c ctxt)) (cexpr e t c))
  (define (do-list es ts) (combine-res (for/list ([v es] [t ts]) (cexpr v t ctxt))))
  (define (new-comp-idx) (gensym 'ci))
  (match e
    [(expr-bind () b) (do-one b type)]
    ;; [(bind (p ...) res)
    ;;  (let* ([binds (do-binds p)]
    ;;         [bind-ctxt (update-ctxt-for-bind ctxt p binds)])
    ;;    (add-bind (do-one res type) binds))]
    [(expr-where body) (do-one body type ctxt)]
    [(expr-where body def)
     (define-values (d-res ctxt^) (cdef def ctxt))
     ;; (define ctxt^ (update-ctxt ctxt #:vals binds))
     (do-res [_ d-res]
             (do-one body type ctxt^))]
    [(expr-app rator (targs ...) vargs ...)
     (let*-values
         ([(rator-val rator-type) (ctxt-lookup-orig ctxt (expr-var-name rator))]
          [(stype pargs) (specialize-poly-type rator-type targs vargs type ctxt)]
          [(argts) (cry-ir-type-fun-vargs stype)]
          [(rator-sval rator-spec) (specialize-rator (expr-var-name rator) rator-val stype pargs ctxt)])
       (add-spec (do-res [args (do-list vargs argts)]
                         (cry-ir-make-expr-app type rator-sval args))
                 rator-spec))]
    [(expr-cond (chk thn) ... els)
     (do-res [chkc (do-list chk (map (const (type-bit)) chk))]
             [thnc (do-list thn (map (const type) thn))]
             [elsc (do-one els type)]
             (cry-ir-make-expr-cnd type chkc thnc elsc))]
    [(expr-var name) (cond [(ctxt-has-val? ctxt name)
                            (eres (ctxt-lookup-val ctxt name))]
                           [else
                            (define-values (oval otyp) (ctxt-lookup-orig ctxt name))
                            (eres (cry-ir-expr-pvar (ctype otyp ctxt) name))]
                           )]
    [(expr-tvar name) (eres (ctxt-lookup-type-var ctxt name))] ;; TODO
    [(expr-annot e t) (do-one e type)]
    [(expr-tuple vs ...)
     (define vts (cry-ir-type-tup-ts type))
     (do-res [cvs (do-list vs vts)]
             (cry-ir-expr-tup type cvs))]
    [(expr-lit i) (eres (cry-ir-expr-lit type i))]
    [(expr-char c) (eres (cry-ir-expr-lit type c))]

    [(expr-sequence-basic vs ...)
     (do-res [vsc (do-list vs (map (const (cry-ir-type-seq-elem type)) vs))]
             (cry-ir-make-expr-seq-val type vsc))]
    [(expr-sequence-enum from step to) (cry-ir-expr-seq-enm type from step to)]
    [(expr-sequence-str s)
     (do-one (make-expr-sequence-basic (map make-expr-lit (bytes->list (string->bytes/locale s)))) type)]
    [(expr-sequence-comp body [(vars vals)] ...)
     (define et (cry-ir-type-seq-elem type))
     (define cidx (new-comp-idx))
     (define var-def-ress
       (for/list ([vr vars] [vl vals])
         (let*-values ([(var-calc-type _vct-vars) (calc-type vl #f ctxt)]
                       [(vl-type) (cry-ir-type-seq-elem var-calc-type)])
           (do-res [var-val (do-one vl vl-type)]
                   (cry-ir-def-val (pat-var-name vr) vl-type (cry-ir-expr-seq-idx vl-type var-val cidx))))))
     (do-res [var-defs (combine-res var-def-ress)]
             [body-val (cexpr body et (update-ctxt ctxt #:vals var-defs))]
             (cry-ir-expr-seq-laz type cidx body-val))]))

(define (ctest t ctxt)
  (printf "compiling-test: ~a\n" (pretty-cry t))
  (match t
    [(def-test name e1 e2)
     (define-values (t2 _2) (calc-type e2 #f ctxt))
     (define-values (t1 _1) (calc-type e1 t2 ctxt))
     (match-define (res v1 b1 s1) (cexpr e1 t2 ctxt))
     (match-define (res v2 b2 s2) (cexpr e2 t2 ctxt))
     (res (cry-ir-def-tst name type (cry-ir-make-expr-bnd b1 v1) (cry-ir-make-expr-bnd b2 v2)) #f '() (append s1 s2))]))

(define (to-cry-ir prog primitives preludes)
  (printf "prog: \n~a\n" prog)
  (define (update-ctxt-prim d c)
    (match d
      [(def-typeof names ... t) (update-ctxt c #:orig (map (λ (name) (env-var-prim-typeof name t)) names))]
      [(def-type name t) (update-ctxt c #:orig (env-var-prim-type name t))]))
  (define (update-ctxt-prelude d c)
    (match d
      [(def-typed-val n t v)
       (update-ctxt c #:orig (env-var-prelude n v t))]))
  (define (update-ctxt-prog d c)
    (match d
      [(def-typed-val n t v)
       (update-ctxt c #:orig (env-var-prog n v t))]))

  (define-values (combined-prims prims-val-env prims-gen-type-env) (gensym-names-defs primitives))
  (match-define (def-combined (prim-type-defs ...) (prim-typeof-defs ...) (prim-val-defs ...) (prim-tests ...)) combined-prims)
  (define-values (combined-prels prel-val-env prel-type-env) (gensym-names-defs preludes))
  (match-define (def-combined (prel-type-defs ...) (prel-typeof-defs ...) (prel-val-defs ...) (prel-tests ...)) combined-prels)
  (define-values (combined-progs prog-val-env prog-type-env) (gensym-names-defs prog))
  (match-define (def-combined (prog-type-defs ...) (prog-typeof-defs ...) (prog-val-defs ...) (prog-tests ...)) combined-progs)

  ;; (define-values (prl-val-defs prl-type-defs prl-typeof-defs prl-test-defs) (split-defs (remove-gen-defs preludes)))
  ;; (define-values (prl-ctvs prl-typeofs) (combine-val&typeof prl-val-defs prl-typeof-defs))

  ;; (define-values (prog-val-defs prog-type-defs prog-typeof-defs prog-test-defs) (split-defs (remove-gen-defs prog)))
  ;; (define-values (prog-ctvs prog-typeofs) (combine-val&typeof prog-val-defs prog-typeof-defs))

  ;; (printf "rest: ~a\n" (list prl-typeofs prl-type-defs prog-type-defs prog-typeofs))
  (define ctxt (foldr
                update-ctxt-prog
                (foldr update-ctxt-prelude (foldr update-ctxt-prim (empty-ctxt) prim-typeof-defs)
                       prel-val-defs)
                prog-val-defs))
  ;; (printf "origs: ~a\n" (cc-origs ctxt))
  (pretty-ctxt ctxt)
  (define tests (map (curryr ctest ctxt) prog-tests))
  (printf "tests: ~a\n" tests)
  '()
  )

;; (define primitive-typeofs-asts (stx-to-cry-ast primitive-typeofs-stx))
;; (define prelude-defs-asts (stx-to-cry-ast prelude-defs-stx))

;; (define (compile-cry asts)
;;   (define-values (prims prims-gen-val-env prims-gen-type-env) (gensym-names-defs primitive-typeofs-asts))
;;   (define (do-prim prim)
;;     (match prim
;;       [(def-typeof names ... t) (map (λ (name) (env-primitive-var name t)) names)]) )
;;   (define prim-env (append (append-map do-prim (def-combined-types prims)) (append-map do-prim (def-combined-tos prims))))
;;   (define-values (combined-prelude prelude-val-env prelude-type-env) (gensym-names-defs prelude-defs-asts))
;;   (define-values (combined-input inp-val-env inp-type-env) (gensym-names-defs asts))
;;   #;(debug (printf "prelude:\n")
;;          (pretty-print (pretty-cry combined-prelude))
;;          (printf "primitives:\n") (pretty-print (pretty-cry prims))
;;          (pretty-print prim-env)
;;          (printf "compiling:\n")
;;          (pretty-print (pretty-cry combined-input)))
;;   (define ctxt
;;     (update-ctxt-for-cdef
;;      combined-input
;;      (update-ctxt-for-cdef combined-prelude
;;                            (update-env (empty-context) #:val prim-env #:typeof prim-env)
;;                            env-prelude-var)))
;;   (values (compile-tests (for/list ([t (def-combined-tests combined-input)])
;;                            (do-cry t ctxt)) ctxt)
;;           ctxt))

;; (module+ test
;;   (require rackunit)
;;   (define bit-id-stx #`(def [id : bit -> bit] [id a = a]))
;;   (define id-any #`(def [id : {a} a -> a] [id a = a]))
;;   (define ti1 #`(test ti1 (== (id true) true)))

;;   (define bit-id-use-any #`(def [bit-id : bit -> bit] [bit-id a = (id a)]))
;;   (define tbi1 #`(test tbi1 (== (bit-id true) true)))

;;   (define pt1 #`(def [pt1 : {a b} #(a b) -> a] [pt1 #(b1 b2) = b1]))
;;   (define tpt1 #`(test tpt1 (== (pt1 #(true (: zero integer))) true)))

;;   (define (compile&print tds)
;;     (define-values (tests ctxt) (compile-cry (map stx-to-cry-ast tds)))
;;     (printf "compiler-result:\n")
;;     (pretty-print tests)
;;     ;; (print-cc ctxt)
;;     (println "lifts:")
;;     (map print-ev (unbox (cc-lifts ctxt))))

;;   (define (tc . d)
;;     (check-not-exn (thunk (compile&print d))))
;;   (tc id-any ti1)
;;   ;; (tc pt1 tpt1)
;;   ;; (tc id-any ti1)
;;   ;; (tc id-any bit-id-use-any tbi1)
;;   )
