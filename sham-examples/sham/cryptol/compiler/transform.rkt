#lang racket

(require racket/syntax)

(require "stx-to-cry-ast.rkt")
(require "../ast.rkt"
         "../prelude/cryptol.rkt"
         "utils.rkt"
         "ctxt.rkt"
         "types.rkt"
         "compiler.rkt"
         "debug.rkt"
         sham/sam/transform
         sham/sam/rkt
         sham/sam/runtime)

(provide compile-cry)

(define (update-ctxt-for-bind ctxt pats body)
  ;; (printf "updating-ctxt-for-bind: ~a ~a\n" pats body) (print-cc ctxt)
  (define ctype (cc-type ctxt))
  (match-define (list farg-types ... res-type) (get-farg-types ctype))
  (define farg-vals
    (build-list (length farg-types) identity)
    ;; (map cons ;; (λ (ft i) (compile-function-arg ft i ctype ctxt))
    ;;      (build-list (length farg-types) identity) farg-types)
    )
  (define res-val (compile-function-result res-type ctype ctxt))
  (define (do-pat pat-ast val typ)
    (match pat-ast
      [(pat-var name) (list (list name (compile-function-arg name val typ ctype ctxt) typ))]
      [(pat-tuple ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)]
                         [t (type-tuple-ts typ)])
                (do-pat p (compile-tuple-index val typ i ctxt) t)))]
      [(pat-sequence ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)])
                (do-pat p (compile-sequence-index val typ i ctxt) (type-sequence-t typ))))]))
  (define pat-vars (append-map do-pat pats farg-vals farg-types))
  (update-context! ctxt
                   #:type res-type
                   #:res res-val
                   #:env (update-env (cc-env ctxt)
                                     #:val (map (λ (po) (env-bind-var (first po) (second po))) pat-vars)
                                     #:typeof (map (λ (po) (env-bind-var (first po) (third po))) pat-vars))))

(define (update-ctxt-for-cdef cdef ctxt (evt env-var))
  (match-define (def-combined (type-defs ...) (typeof-defs ...) (tv-defs ...) (test-defs ...)) cdef)
  ;; (unless (empty? test-defs) (println "please filter out tests first"))
  (define typeof-env (flatten (for/list ([d typeof-defs])
                        (map (λ (name) (evt name (def-typeof-val d))) (def-typeof-names d)))))
  (define type-env (for/list ([d type-defs]) (evt (def-type-name d) (def-type-val d))))
  (define new-ctxt
    (update-context! ctxt
                     #:env (update-env (and ctxt (cc-env ctxt)) #:typeof typeof-env #:type type-env)))
  (letrec
      ([vals-ctxt
        (for/fold ([vc new-ctxt])
                  ([tv tv-defs])
          (match-define (def-typed-val name type value) tv)
          (update-env
           vc
           #:typeof (env-var name type)
           #:val
           (env-lazy-var name
                         (λ (pargs vargs app-ctxt)
                           (do-typed-val name type value vals-ctxt pargs vargs app-ctxt))
                         value)))])
    vals-ctxt))

(define (update-ctxt-for-where cdef ctxt)
  (match-define (def-combined (type-defs ...) (typeof-defs ...) (otv-defs ...) (test-defs ...)) cdef)
  (define typeof-env (flatten (for/list ([d typeof-defs])
                                (map (λ (name) (env-var name (try-specialize-type (def-typeof-val d) ctxt)))
                                     (def-typeof-names d)))))
  (define type-env (for/list ([d type-defs]) (env-var (def-type-name d) (def-type-val d))))
  (define new-ctxt
    (update-context! ctxt
                     #:env (update-env (and ctxt (cc-env ctxt)) #:typeof typeof-env #:type type-env)))

  ;; fold over values twice for mutually recursive sequences, first just for env next for building value
  ;; (define val-ctxt
  ;;   (for/fold ([vc new-ctxt])
  ;;             ([tv tv-defs])
  ;;     (match-define (def-typed-val name type value) tv)
  ;;     (update-env vc
  ;;                 #:typeof (env-var name type)
  ;;                 #:val (env-var name (compile-where-val-env name type value vc)))))
  (define tv-defs (for/list ([tv otv-defs])
                    (match-define (def-typed-val name type value) tv)
                    (def-typed-val name (cdr (maybe-calc-type value type new-ctxt)) value)))
  (define tv-typeofs (for/list ([tv tv-defs]) (env-where-var (def-typed-val-name tv) (def-typed-val-type tv))))
  (define-values (val-ctxt prep-tvs) (compile-where-val-env tv-defs (update-env new-ctxt #:typeof tv-typeofs)))

  (for/fold ([vc val-ctxt])
            ([tv tv-defs]
             [pv prep-tvs])
    (match-define (def-typed-val name type value) tv)
    (define cres (compile-where-val-res name type pv ctxt))
    (define val (match value [(expr-bind () v) v] [else value]))
    (printf "compiling-where: ~a ~a\n" (pretty-cry val) cres)
    (define cval (do-cry val (update-context! vc #:type type #:res cres)))
    (update-env vc #:val (env-where-var name (compile-where-val name type pv cval ctxt)))))

;; this creates a lazy compiler which compiles when intrinsic arguments are given and per application
(define (do-typed-val name type val orig-ctxt pargs vargs app-ctxt)
  ;; (define val-type (lookup-typeof orig-ctxt name))
  (debug (printf "forcing-lazy: ~a\n  val:~a\n  pargs:~a\n  vargs:~a\n  ret-type:~a\n"
                 name (pretty-cry val) pargs vargs (cc-type orig-ctxt)))
  (define-values (pargs-evs up-type) (specialize-poly-type type pargs vargs app-ctxt))
  (printf "pargs-evs: ~a\n" pargs-evs)
  (let* ([new-name (ast-id-gen name)]
         [new-env (update-env (cc-env orig-ctxt) #:tvar pargs-evs)]
         [cctxt (compile-internal-def-context name type orig-ctxt)]
         [new-ctxt (update-context! orig-ctxt #:type up-type #:env new-env #:cc cctxt)]
         [cval (do-cry val new-ctxt)]
         [cdef (compile-def-val name new-name up-type type pargs-evs cval cctxt new-ctxt)]
         [svar (env-special-var new-name cdef up-type name type pargs-evs)])
    (add-lifts! orig-ctxt svar)
    svar))

(define-transform (do-cry (ctxt #f))
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        ;; [(val name body)
        ;;  (let ([val-ctxt (update-ctxt-for-def-val name body ctxt)])
        ;;    (do-def-val name body val-ctxt))]
        [(test name e1 e2)
         (define type (cdr (maybe-calc-type e1 (cdr (maybe-calc-type e2 #f ctxt)) ctxt)))
         (define tc (compile-internal-def-context name type ctxt))
         (match-define (cons re1 re2) (compile-def-test-results type (update-context! ctxt #:cc tc)))
         (compile-def-test name
                           type
                           (cexpr e1 (update-context! ctxt #:res re1 #:cc tc #:type type))
                           re1
                           (cexpr e2 (update-context! ctxt #:res re2 #:cc tc #:type type))
                           re2
                           (update-context! ctxt #:cc tc))])

  (cexpr (expr -> any)
         [(bind (p ...) res)
          (let ([bind-ctxt (update-ctxt-for-bind ctxt p res)])
            (compile-expr-bind (cexpr res bind-ctxt) bind-ctxt))]
         [(app rator (targs ...) vargs ...)
          (printf "cexpr-app: ~a\n" (pretty-cry this-ast))
          ;; (debug (printf "app: ~a ~a ~a\n" (cons rator (lookup-val ctxt (expr-var-name rator))) targs vargs))
          (let ([rator-val
                 (lookup-val-env ctxt (expr-var-name rator))
                 ;; (car (lookup-env-vars (env-val (cc-env ctxt)) (expr-var-name rator)))
                 ])
            (define (compile-arg arg-expr arg-type arg-res)
              (define calc-type (cdr (maybe-calc-type arg-expr (try-specialize-type arg-type ctxt) ctxt)))
              (cexpr arg-expr (update-context! ctxt #:res arg-res #:type calc-type)))
            (define (compile-args args up-type)
              (for/list ([arg args]
                         [argt (drop-right (get-farg-types up-type) 1)]
                         [i (length args)])
                (define app-arg-result (compile-expr-result argt #f ctxt))
                (cons app-arg-result (cexpr arg (update-context! ctxt #:res app-arg-result #:type argt)))))
            (match rator-val
              [(env-primitive-var rator-name rator-pval)
               (define-values (pvar-args rator-up-type)
                 (specialize-poly-type (lookup-typeof ctxt rator-name) targs vargs ctxt))
               ;; (define compiled-vargs (compile-args vargs rator-up-type))
               (compile-expr-app-primitive rator-val pvar-args rator-up-type vargs compile-arg ctxt)]
              [(env-lazy-var name valf ast)
               (let* ([compiled-rator (valf targs vargs ctxt)]
                      [new-rator-type (env-special-var-type compiled-rator)]
                      [compiled-vargs (compile-args vargs new-rator-type)])
                 (compile-expr-app compiled-rator compiled-vargs ctxt))]
              [#f (error 'cryptol/compile "app rator not found ~a" (pretty-cry rator))]
              [else (error 'cryptol/compile "unknown app rator: ~a ~a" rator-val this-ast)]))]
         [(cond ((^ chk) (^ thn)) ... (^ els)) (compile-expr-cond chk thn els ctxt)]
         [(var name) (compile-expr-var name (lookup-val-env ctxt name) ctxt)]
         [(tvar name) (compile-expr-tvar name (lookup-type ctxt name) ctxt)]
         [(annot e t) (cexpr e (update-context! ctxt #:type t))]
         [(where (^ body)) body]
         [(where body ds) (cexpr body (update-ctxt-for-where ds ctxt))]
         [(error msg) (compile-error-msg msg ctxt)]

         [(tuple vs ...)
          (printf "tuple: ~a ~a\n" (pretty-cry this-ast) (pretty-cry (cc-type ctxt)))
          (let* ([type (cc-type ctxt)]  ;; if needed maybe-calc-type
                 [orig-result (cc-res ctxt)]
                 [cvs (for/list ([v vs] [i (length vs)])
                        (define arg-type (maybe-tuple-type-i type i))
                        (define result (compile-expr-result arg-type (and orig-result (compile-tuple-index orig-result type i ctxt)) ctxt))
                        (printf "tuple-fields: ~a ~a ~a\n" (pretty-cry v) (pretty-cry arg-type) result)
                        (cexpr v (update-context! ctxt #:type arg-type #:res result)))])
            (compile-tuple-literal cvs ctxt))]
         [(lit i) (compile-integer-literal i ctxt)]
         [(char c) (compile-char-literal c ctxt)])

  (cseq (sequence -> any)
        [(basic vs ...)
         (printf "seq: ~a ~a ~a\n" (pretty-cry this-ast) (pretty-cry (cc-type ctxt)) (cc-res ctxt))
         (let* ([type (cc-type ctxt)]  ;; if needed maybe-calc-type
                [orig-result (cc-res ctxt)]
                [cvs (for/list ([v vs] [i (length vs)])
                       (define arg-type (maybe-sequence-elem-type type))
                       (define result (compile-expr-result arg-type (and orig-result (compile-sequence-index-result orig-result type i ctxt)) ctxt))
                       (printf "basic-sequence-val: ~a ~a ~a ~a\n" (pretty-cry v) (pretty-cry arg-type) orig-result result)
                       (cexpr v (update-context! ctxt #:type arg-type #:res result)))])
           (compile-sequence-basic cvs ctxt))]
        [(enum from step to) (compile-sequence-enum from step to ctxt)] ;; compile from,step,to with correct type for complicated enums
        [(str s)
         (cexpr (make-expr-sequence-basic (map make-expr-lit (bytes->list (string->bytes/locale s))))
                (update-context! ctxt #:type (type-sequence (dim-int (string-length s))
                                                            (type-sequence (dim-int 8) (type-bit)))))
         ;; (compile-sequence-string s)
         ]
        [(comp body [(vars vals)] ...)
         (match-define (cons typed-vars calc-type) (maybe-calc-type this-ast (cc-type ctxt) ctxt))
         ;; (printf "calc-type: ~a ~a\n" vars calc-type)
         (let* ([cvvs
                 (for/list ([vr vars] [vl vals])
                   (define vl-elem-type (maybe-first-env-vars (lookup-env-vars typed-vars (pat-var-name vr))))
                   (define vlt (maybe-calc-type vl (type-sequence #f (or vl-elem-type (type-unknown))) (update-context! ctxt #:env (update-env (cc-env ctxt) #:tvar typed-vars))))
                   ;; (define res (compile-expr-result (cdr vlt) #f ctxt))
                   (define cvl (cexpr vl (update-context! ctxt #:type (cdr vlt) #:res #f)))
                   (list vr #f cvl (cdr vlt)))]
                [comp-index (compile-sequence-comp-index calc-type ctxt)]
                [vvs-env (for/list ([vv cvvs]) (env-var (pat-var-name (first vv)) (compile-sequence-comp-var vv comp-index ctxt)))]
                [comp-result (compile-sequence-comp-result comp-index calc-type ctxt)]
                [body-ctxt (update-context! ctxt
                                            #:env (update-env (cc-env ctxt) #:val vvs-env)
                                            #:res comp-result
                                            #:type (maybe-sequence-elem-type calc-type))]
                [compiled-body (cexpr body body-ctxt)])
           (compile-sequence-comp compiled-body cvvs comp-index comp-result (update-context! ctxt #:type calc-type)))]))


(define primitive-typeofs-asts (stx-to-cry-ast primitive-typeofs-stx))
(define prelude-defs-asts (stx-to-cry-ast prelude-defs-stx))

(define (compile-cry asts)
  (define-values (prims prims-gen-val-env prims-gen-type-env) (gensym-names-defs primitive-typeofs-asts))
  (define (do-prim prim)
    (match prim
      [(def-typeof names ... t) (map (λ (name) (env-primitive-var name t)) names)]) )
  (define prim-env (append (append-map do-prim (def-combined-types prims)) (append-map do-prim (def-combined-tos prims))))
  (define-values (combined-prelude prelude-val-env prelude-type-env) (gensym-names-defs prelude-defs-asts))
  (define-values (combined-input inp-val-env inp-type-env) (gensym-names-defs asts))
  #;(debug (printf "prelude:\n")
         (pretty-print (pretty-cry combined-prelude))
         (printf "primitives:\n") (pretty-print (pretty-cry prims))
         (pretty-print prim-env)
         (printf "compiling:\n")
         (pretty-print (pretty-cry combined-input)))
  (define ctxt
    (update-ctxt-for-cdef
     combined-input
     (update-ctxt-for-cdef combined-prelude
                           (update-env (empty-context) #:val prim-env #:typeof prim-env)
                           env-prelude-var)))
  (values (compile-tests (for/list ([t (def-combined-tests combined-input)])
                           (do-cry t ctxt)) ctxt)
          ctxt))

(module+ test
  (require rackunit)
  (define bit-id-stx #`(def [id : bit -> bit] [id a = a]))
  (define id-any #`(def [id : {a} a -> a] [id a = a]))
  (define ti1 #`(test ti1 (== (id true) true)))

  (define bit-id-use-any #`(def [bit-id : bit -> bit] [bit-id a = (id a)]))
  (define tbi1 #`(test tbi1 (== (bit-id true) true)))

  (define pt1 #`(def [pt1 : {a b} #(a b) -> a] [pt1 #(b1 b2) = b1]))
  (define tpt1 #`(test tpt1 (== (pt1 #(true (: zero integer))) true)))

  (define (compile&print tds)
    (define-values (tests ctxt) (compile-cry (map stx-to-cry-ast tds)))
    (printf "compiler-result:\n")
    (pretty-print tests)
    ;; (print-cc ctxt)
    (println "lifts:")
    (map print-ev (unbox (cc-lifts ctxt))))

  (define (tc . d)
    (check-not-exn (thunk (compile&print d))))
  (tc id-any ti1)
  ;; (tc pt1 tpt1)
  ;; (tc id-any ti1)
  ;; (tc id-any bit-id-use-any tbi1)
  )
