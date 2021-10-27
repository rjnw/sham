#lang racket

(require racket/syntax)

(require "stx-to-cry-ast.rkt")
(require "../ast.rkt"
         "../prelude.rkt"
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
  (define (do-pat pat-ast val typ)
    (match pat-ast
      [(pat-var name) (list (list name val typ))]
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
                (do-pat p (compile-sequence-index val i ctxt) (type-sequence-t typ))))]))
  (define ctype (cc-type ctxt))
  (match-define (list farg-types ... res-type) (get-farg-types ctype))
  (define farg-vals (map (λ (ft i) (compile-function-arg ft i ctype ctxt))
                         farg-types
                         (build-list (length farg-types) identity)))
  (define res-val (compile-function-result res-type ctype ctxt))
  (define pat-vars (append-map do-pat pats farg-vals farg-types))
  (update-context! ctxt
                   #:type res-type
                   #:res res-val
                   #:env (update-env (cc-env ctxt)
                                     #:val (map (λ (po) (env-var (first po) (second po))) pat-vars)
                                     #:typeof (map (λ (po) (env-var (first po) (third po))) pat-vars))))

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

;; this creates a lazy compiler which compiles when intrinsic arguments are given and per application
(define (do-typed-val name type val orig-ctxt pargs vargs app-ctxt)
  ;; (define val-type (lookup-typeof orig-ctxt name))
  (debug (printf "forcing-lazy: ~a\n  val:~a\n  pargs:~a\n  vargs:~a\n  ret-type:~a\n"
                 name (pretty-cry val) pargs vargs (cc-type orig-ctxt)))
  (define-values (pargs-evs up-type) (specialize-poly-type type pargs vargs app-ctxt))
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
               (define compiled-vargs (compile-args vargs rator-up-type))
               (compile-expr-app-primitive rator-name rator-pval pvar-args compiled-vargs ctxt)]
              [(env-lazy-var name valf ast)
               (let* ([compiled-rator (valf targs vargs ctxt)]
                      [new-rator-type (env-special-var-type compiled-rator)]
                      [compiled-vargs (compile-args vargs new-rator-type)])
                 (compile-expr-app compiled-rator compiled-vargs ctxt))]))]
         [(cond ((^ chk) (^ thn)) ... (^ els)) (compile-expr-cond chk thn els ctxt)]
         [(var name) (compile-expr-var name (lookup-val-env ctxt name) ctxt)]
         [(tvar name) (compile-expr-tvar name (lookup-type ctxt name) ctxt)]
         [(annot e t) (cexpr e (update-context! ctxt #:type t))]
         [(where (^ body)) body]
         [(where body ds) (cexpr body (update-ctxt-for-cdef ds ctxt))]
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
        [(basic (^ vs) ...) (compile-sequence-basic vs ctxt)]
        [(enum (^ from) (^ step) (^ to)) (compile-sequence-enum from step to)]
        [(str s) (compile-sequence-string s)]
        [(comp body [(vars (^ vals #:ctxt (update-context! ctxt #:type #f)))] ...)
         (let* ([compiled-vvs
                 (for/list ([vr vars] [vl vals])
                   (env-var vr (compile-sequence-var vr vl ctxt)))]
                [comp-ctxt (update-context! ctxt #:env (update-env (cc-env ctxt) #:val compiled-vvs))]
                [compiled-body (cexpr body comp-ctxt)])
           (compile-sequence-comp compiled-body compiled-vvs comp-ctxt))]))


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
  (debug (printf "prelude:\n")
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
