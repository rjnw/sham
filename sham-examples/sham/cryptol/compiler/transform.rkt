#lang racket

(require racket/syntax)

(require "../ast.rkt"
         "utils.rkt"
         "ctxt.rkt"
         "types.rkt"
         "compiler.rkt"
         sham/sam/transform
         sham/sam/rkt
         sham/sam/runtime)

;; (define sham-basic-integer-type #`i64)
;; (define sham-result-type sham-basic-integer-type) ;TODO do something to add error message as well
;; (define sham-result-arg #`res)

(define (update-ctxt-for-def-val name body ctxt)
  (define-values (uptype pvars cs) (unwrap-poly (lookup-typeof ctxt name)))
  ;; TODO use constraints
  (update-context! ctxt #:type uptype #:pvars pvars))

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
  (debug (printf "updating-ctxt-for-bind: pat-vars:") (map print-ev pat-vars) (newline))
  (print-cc (update-context! ctxt
                    #:type res-type
                    #:res res-val
                    #:env (update-env (cc-env ctxt) #:val pat-vars))))
(define (update-ctxt-for-where defs ctxt)
  ctxt)

;; this creates a lazy compiler which compiles when intrinsic arguments are given and per application
(define (lazy-compile-def-val name val orig-ctxt)
  (define val-type (lookup-typeof orig-ctxt name))
  (define-values (uptype pvars cs) (unwrap-poly val-type orig-ctxt))
  (λ (pargs vargs app-ctxt)
    (let* ([maybe-result-type (cc-type app-ctxt)]
           [maybe-varg-types (map (curryr maybe-calc-type app-ctxt) vargs)]

           [pvar-binds (figure-out-pvars pvars cs pargs maybe-varg-types)]

           [new-name (ast-id-gen name)]
           [new-type (specialize-type val-type pvar-binds maybe-varg-types)]

           [new-env (update-env (cc-env orig-ctxt) #:type pvar-binds)]
           [new-ctxt (update-context! orig-ctxt #:type new-type #:env new-env)]
           [compiled-val (compile-cry val new-ctxt)]
           [compiled-def (compile-def-val name new-type compiled-val orig-ctxt)]

           [svar (env-svar new-name compiled-val new-type name val-type pargs)])
      (add-lifts! orig-ctxt svar)
      svar)))

(define (update-annot-ctxt ctxt ast) ctxt)
(define (update-where-ctxt ctxt ast) ctxt)
(define (integer-literal i ctxt) i)
(define (char-literal c ctxt) c)
(define (zero-literal ctxt) 0)

(define-transform (compile-cry (ctxt #f))
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        [(val name body)
         (let ([val-ctxt (update-ctxt-for-def-val name body ctxt)])
           (lazy-compile-def-val name body val-ctxt))])
  (cexpr (expr -> any)
         [(bind (p ...) res)
          (let ([bind-ctxt (update-ctxt-for-bind ctxt p res)])
            (compile-expr-bind (cexpr res bind-ctxt) bind-ctxt))]
         [(app rator (targs ...) vargs ...)
          (let* ([rator-lazy-val (lookup-val ctxt rator)]
                 [compiled-rator (rator-lazy-val targs vargs ctxt)]
                 [new-rator-type (env-var-type compiled-rator)]
                 [compiled-vargs
                  (for/list ([varg vargs]
                             [vargt (get-farg-types new-rator-type)])
                    (cexpr varg (update-context! ctxt #:type vargt)))])
            (compile-expr-app (env-var-name compiled-rator) compiled-vargs ctxt))]
         [(cond ((^ chk) (^ thn)) ... (^ els)) (compile-expr-cond chk thn els ctxt)]
         [(var name) (compile-expr-var (lookup-val ctxt name) ctxt)]
         [(tvar name) (compile-expr-tvar (lookup-type ctxt name) ctxt)]
         [(annot e t) (cexpr e (update-context! ctxt #:type t))]
         [(where body ds ...) (cexpr body (update-ctxt-for-where ds ctxt))]
         [(error msg) (compile-error-msg msg ctxt)]

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

(define (compile-defs defs (ctxt #f)) ;TODO
  (define-values (val-defs type-defs typeof-defs test-defs) (split-defs (remove-gen-defs defs)))
  (define combined-vals (combine-val&typeof val-defs typeof-defs))
  (define typeof-env (for/list ([d typeof-defs]) (env-var (def-typeof-name d) #f (def-typeof-val d))))
  (printf "typeof-env: ~a\n" typeof-env)
  (define type-env (for/list ([d type-defs]) (env-var (def-type-name d) (def-type-val d) #f)))
  (printf "type-env: ~a\n" type-env)
  (define new-ctxt
    (update-context! ctxt
                     #:env (update-env (and ctxt (cc-env ctxt)) #:val typeof-env #:type type-env)))
  (for/list ([vt combined-vals])
    (compile-cry (car vt) (update-context! new-ctxt #:type (def-typeof-val (cdr vt))))))

(module+ test
  (require "../lang/stx-to-cry-ast.rkt")
  (define bit-id-stx #`(def [id : bit -> bit] [id a = a]))
  (define id-any #`(def [id : {a} a -> a] [id a = a]))
  (define bit-use-any #`(def [bit-id : bit -> bit] [bit-id a = id a]))

  (define pt1 #`(def [pt1 : #(bit bit) -> bit] [pt1 #(b1 b2) = b1]))

  ;; (define bit-id-cast (stx-to-cry-ast bit-id-stx))
  ;; (println bit-id-cast)

  (define (tc d)
    (with-handlers ([exn:fail? (λ (e) (displayln e) ((error-display-handler) "error-compiling" e))])
      (println (compile-defs (list (stx-to-cry-ast d))))))
  (tc bit-id-stx)
  (tc pt1))
