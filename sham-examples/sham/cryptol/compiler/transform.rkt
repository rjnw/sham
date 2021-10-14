#lang racket

(require racket/syntax)

(require "stx-to-cry-ast.rkt")
(require "../ast.rkt"
         "utils.rkt"
         "ctxt.rkt"
         "types.rkt"
         "compiler.rkt"
         sham/sam/transform
         sham/sam/rkt
         sham/sam/runtime)

(provide compile-defs)

(define (update-ctxt-for-bind ctxt pats body)
  ;; (printf "updating-ctxt-for-bind: ~a ~a\n" pats body) (print-cc ctxt)
  (define (do-pat pat-ast val typ)
    (match pat-ast
      [(pat-var name) (list (env-var name val typ))]
      [(pat-tuple ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)]
                         [t (type-tuple-ts typ)])
                (do-pat p (compile-tuple-index val i ctxt) t)))]
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
  ;; (debug (printf "updating-ctxt-for-bind: pat-vars:") (map print-ev pat-vars) (newline))
  (print-cc (update-context! ctxt
                    #:type res-type
                    #:res res-val
                    #:env (update-env (cc-env ctxt) #:val pat-vars))))

(define (update-ctxt-for-defs defs ctxt)
  (define-values (val-defs type-defs typeof-defs test-defs) (split-defs (flatten (remove-gen-defs defs))))
  (unless (empty? test-defs) (error 'sham/cryptol/ "please filter out tests first"))
  (debug (println "compiling-defs:")
         (map println val-defs)
         (map println type-defs)
         (map println typeof-defs)
         (map println test-defs))
  (define combined-vals (combine-val&typeof val-defs typeof-defs))
  (define typeof-env (flatten (for/list ([d typeof-defs])
                        (map (λ (name) (env-var name #f (def-typeof-val d))) (def-typeof-names d)))))
  (define type-env (for/list ([d type-defs]) (env-var (def-type-name d) (def-type-val d) #f)))
  (define new-ctxt
    (update-context! ctxt
                     #:env (update-env (and ctxt (cc-env ctxt)) #:val typeof-env #:type type-env)))
  (letrec
      ([vals-ctxt
        (for/fold ([vc new-ctxt])
                  ([vt combined-vals])
          (match-define (def-val name value) (car vt))
          (update-env
           vc
           #:val
           (env-lazy-var name
                         (λ (pargs vargs app-ctxt)
                           (do-def-val name value vals-ctxt pargs vargs app-ctxt))
                         (def-typeof-val (cdr vt)))))])
    vals-ctxt))

(define (do-type-foo orig-type poly-args value-args curr-ctxt))
;; this creates a lazy compiler which compiles when intrinsic arguments are given and per application
(define (do-def-val name val orig-ctxt pargs vargs app-ctxt)
  (debug (printf "forcing-lazy: ~a\n  val:~a\n  pargs:~a\n  vargs:~a" name val pargs vargs))
  (define val-type (lookup-typeof orig-ctxt name))
  (define-values (uptype pvars cs) (unwrap-poly val-type))
  (let* ([maybe-result-type (cc-type app-ctxt)]
         [maybe-varg-types (map (curryr maybe-calc-type app-ctxt) vargs (drop-right (get-farg-types uptype) 1))]

         [pvar-args (figure-out-pargs val-type pargs maybe-varg-types maybe-result-type app-ctxt)]

         [new-name (ast-id-gen name)]
         [new-type (specialize-type uptype pvar-args maybe-varg-types maybe-result-type orig-ctxt)]

         [new-env (update-env (cc-env orig-ctxt) #:type pvar-args)]
         [new-ctxt (update-context! orig-ctxt #:type new-type #:env new-env)]
         [compiled-val (do-cry val new-ctxt)]
         [compiled-def (compile-def-val name new-type compiled-val orig-ctxt)]

         [svar (env-special-var new-name compiled-val new-type name val-type pargs)])
    (add-lifts! orig-ctxt svar)
    svar))

(define-transform (do-cry (ctxt #f))
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        ;; [(val name body)
        ;;  (let ([val-ctxt (update-ctxt-for-def-val name body ctxt)])
        ;;    (do-def-val name body val-ctxt))]
        [(test name (^ e1) (^ e2)) (compile-def-test name e1 e2 ctxt)])

  (cexpr (expr -> any)
         [(bind (p ...) res)
          (let ([bind-ctxt (update-ctxt-for-bind ctxt p res)])
            (compile-expr-bind (cexpr res bind-ctxt) bind-ctxt))]
         [(app rator (targs ...) vargs ...)
          (debug (printf "app: ~a ~a ~a\n" rator targs vargs))
          (let ([rator-val (lookup-val ctxt (expr-var-name rator))])
            (match rator-val
              [(env-primitive-var name #f type)
               (compile-expr-app-primitive rator-val targs vargs)]
              [(env-lazy-var name valf type)
               (let* ([compiled-rator (valf targs vargs ctxt)]
                      [new-rator-type (env-var-type compiled-rator)]
                      [compiled-vargs
                       (for/list ([varg vargs]
                                  [vargt (get-farg-types new-rator-type)])
                         (cexpr varg (update-context! ctxt #:type vargt)))])
                 (compile-expr-app (env-var-name compiled-rator) compiled-vargs ctxt))]))]
         [(cond ((^ chk) (^ thn)) ... (^ els)) (compile-expr-cond chk thn els ctxt)]
         [(var name) (compile-expr-var name (lookup-val ctxt name) ctxt)]
         [(tvar name) (compile-expr-tvar name (lookup-type ctxt name) ctxt)]
         [(annot e t) (cexpr e (update-context! ctxt #:type t))]
         [(where body ds ...) (cexpr body (update-ctxt-for-defs ds ctxt))]
         [(error msg) (compile-error-msg msg ctxt)]

         [(tuple (^ vs) ...) (compile-tuple-literal vs ctxt)]
         [(lit i) (compile-integer-literal i ctxt)]
         [(char c) (compile-char-literal c ctxt)]
         [(zero) (compile-zero-literal ctxt)])

  (cseq (sequence -> any)
        [(basic (^ vs) ...) (compile-sequence-basic vs ctxt)]
        [(enum (^ from) (^ step) (^ to)) (compile-sequence-enum from step to)]
        [(str s) (compile-sequence-string s)]
        [(comp body [(vars (^ vals #:ctxt (update-context! ctxt #:type #f)))] ...)
         (let* ([compiled-vvs
                 (for/list ([vr vars] [vl vals])
                   (env-var vr (compile-sequence-var vr vl ctxt) #f))]
                [comp-ctxt (update-context! ctxt #:env (update-env (cc-env ctxt) #:val compiled-vvs))]
                [compiled-body (cexpr body comp-ctxt)])
           (compile-sequence-comp compiled-body compiled-vvs comp-ctxt))]))



(define primitive-typeofs (stx-to-cry-ast primitive-typeofs-stx))
(define prelude-defs (stx-to-cry-ast prelude-defs-stx))

(define (compile-cry asts)
  (define prim-typeofs (flatten (remove-gen-defs defs)))
  (define prim-env
    (flatten (for/list ([p prim-typeofs])
               (map (λ (name) (env-primitive-var name #f (def-typeof-val p))) (def-typeof-names d)))))

  (define-values (val-defs type-defs typeof-defs test-defs) (split-defs (flatten (remove-gen-defs asts))))
  (define ctxt
    (update-ctxt-for-defs
     (append prelude-defs type-defs typeof-defs val-defs)
     (update-env (empty-context) #:vals prim-env)))
  (compile-tests (for/list ([t test-defs]) (do-cry t ctxt)) ctxt))

(module+ test
  (require "stx-to-cry-ast.rkt")
  (require rackunit)
  (define bit-id-stx #`(def [id : bit -> bit] [id a = a]))
  (define id-any #`(def [id : {a} a -> a] [id a = a]))
  (define ti1 #`(test ti1 (== (id (: true bit)) (: true bit))))

  (define bit-id-use-any #`(def [bit-id : bit -> bit] [bit-id a = (id a)]))
  (define tbi1 #`(test tbi1 (== (bit-id true) true)))

  (define pt1 #`(def [pt1 : {a b} #(a b) -> a] [pt1 #(b1 b2) = b1]))
  (define tpt1 #`(test tpt1 (== (pt1 #((: true bit) (: false bit))) true)))

  (define (print-result td)
    (match-define (cons tests c) td)
    (pretty-print tests)
    (print-cc c)
    (pretty-print (unbox (cc-lifts c))))
  (define (tc . d)
    (check-not-exn (thunk (print-result (compile-defs (map stx-to-cry-ast d))))))
  (tc bit-id-stx)
  (tc pt1 tpt1)
  (tc id-any ti1)
  (tc id-any bit-id-use-any tbi1))
