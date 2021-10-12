#lang racket

(require racket/syntax)

(require "../ast.rkt"
         "utils.rkt"
         "types.rkt"
         sham/sam/transform
         sham/sam/rkt
         sham/sam/runtime)

(define sham-basic-integer-type #`i64)
(define sham-result-type sham-basic-integer-type) ;TODO do something to add error message as well
(define sham-result-arg #`res)

(define (update-ctxt-for-def-val name body ctxt)
  (define-values (uptype pvars cs) (unwrap-poly (lookup-typeof ctxt name)))
  ;; TODO use constraints
  (update-context! ctxt #:type uptype #:pvars pvars))

(define (update-ctxt-for-bind ctxt pats body)
  ;; (printf "updating-ctxt-for-bind: ~a ~a\n" pats body) (print-cc ctxt)
  (define (pat-bind pat-ast val typ)
    (match pat-ast
      [(pat-var name) (list (list name val typ))]
      [(pat-tuple ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)]
                         [t (type-tuple-ts typ)])
                (pat-bind p (compile-tuple-index val i ctxt) t)))]
      [(pat-sequence ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)])
                (pat-bind p (compile-sequence-index val i ctxt) (type-sequence-t typ))))]))
  (define ctype (cc-type ctxt))
  (match-define (list farg-types ... res-type) (get-farg-types ctype))
  (define farg-vals (map (λ (ft i) (compile-function-arg ft i ctype ctxt))
                         farg-types
                         (build-list (length farg-types) identity)))
  (define res-val (compile-function-result res-type ctype ctxt))
  ;; (printf "binds: ~a, ~a, res: ~a\n" pats fargs-vals res-val)
  (define pat-binds (append-map pat-bind pats farg-vals farg-types))
  ;; (printf "updating-ctxt-bind: both-pat-binds: ~a\n" pat-binds)
  (print-cc (update-context! ctxt
                    #:type res-type
                    #:res res-val
                    #:env (update-env (cc-env ctxt)
                                      #:val (map (λ (l) (list (first l) (second l))) pat-binds)
                                      #:typeof (map (λ (l) (list (first l) (third l))) pat-binds)))))

(define (compile-val-defs defs (ctxt #f))
  (define-values (val-defs type-defs typeof-defs test-defs) (split-defs (remove-gen-defs defs)))
  (define combined-vals (combine-val&typeof val-defs typeof-defs))
  (define typeof-env (for/list ([d typeof-defs]) (cons (def-typeof-name d) (def-typeof-val d))))
  (printf "typeof-env: ~a\n" typeof-env)
  (define type-env (for/list ([d type-defs]) (cons (def-type-name d) (def-type-val d))))
  (printf "type-env: ~a\n" type-env)
  (define new-ctxt
    (update-context! ctxt
                     #:env (update-env (and ctxt (cc-env ctxt)) #:typeof typeof-env #:type type-env)))
  (for/list ([vt combined-vals])

    (ccry (car vt) (update-context! new-ctxt #:type (def-typeof-val (cdr vt))))))

(struct cmp-def [id])

;; this creates a lazy compiler which compiles when intrinsic arguments are given and per application
(define (lazy-compile-def-val name body orig-ctxt)
  (define type (lookup-typeof name orig-ctxt))
  (define-values (uptype pvars cs) (unwrap-poly type orig-ctxt))
  (λ (pargs vargs curr-ctxt)
    (let* ([maybe-result-type (cc-type curr-ctxt)]
           [maybe-varg-types (map (curryr maybe-calc-type curr-ctxt) vargs)]

           [pvar-binds (figure-out-pvars pvars cs pargs maybe-varg-types)]

           [new-name (ast-id-gen name)]
           [new-env (update-env (cc-env ctxt) #:typeof pvar-binds)]
           [new-ctxt (update-context! ctxt #:type uptype #:env new-env
                                      #:lifts)]))

    (define cmp-val (ccry val new-ctxt))
    (define cmp-def (compile-def-val name new-type cmp-val orig-ctxt))
    (cons new-name cmp-val)))

(define sham-sequence-length-type #`i64)
(define (sham-sequence-type sub-type)
  #`(ll-type-struct #,sham-sequence-length-type (ll-type-pointer #,sub-type)))

(define (sham-int-type width)
  (when (> width 64) (error 'sham/cryptol/TODO "larger than register width for ints"))
  ;; force every bit width less than 64 to 64 for now
  sham-basic-integer-type)
(define (sham-tuple-type subs) #`(ll-type-struct #,@subs))

(define (to-sham-type c-type)
  (match c-type
    [(type-bit) sham-basic-integer-type]
    [(type-sequence (dim-int i) type-bit) (sham-int-type i)]
    [(type-sequence dim t) (sham-sequence-type (to-sham-type t))]
    [(type-tuple ts ...) (sham-tuple-type (map to-sham-type ts))]))

;; for destinition passing style, actual returned type is boxed in pointer and taken as
;;  input wheres sham functions return something for error
(define (sham-def-type ctxt ast)
  (match-define (def-val name body) ast)
  (define cry-type (lookup-typeof ctxt name))
  (define-values (uptype vs cs) (unwrap-poly cry-type))
  (define intrinsic-types (map (λ (i) #`i64) vs))

  (match-define (list fargs ... rett) (get-farg-types uptype))
  (define arg-types (map to-sham-type fargs))
  (define ret-type #`(ll-type-pointer #,(to-sham-type rett)))

  #`(ll-make-type-function (list #,@intrinsic-types #,@arg-types) #f #,sham-result-type))

(define (update-annot-ctxt ctxt ast) ctxt)
(define (update-where-ctxt ctxt ast) ctxt)
(define (integer-literal i ctxt) i)
(define (char-literal c ctxt) c)
(define (zero-literal ctxt) 0)

(define-transform (ccry (ctxt #f))
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        [(val name body)
         (let ([val-ctxt (update-ctxt-for-def-val name body ctxt)])
           (lazy-compile-def-val name body val-ctxt))])
  (cexpr (expr -> any)
         [(bind (p ...) res)
          (let ([bind-ctxt (update-ctxt-for-bind ctxt p res)])
            (compile-bind (cexpr res bind-ctxt) bind-ctxt))]
         [(app o (i ...) a ...)
          (define oval (lookup-val ctxt o)) ;; value for defs in env is a function that specialize-compile
          (define coval (oval i a ctxt))    ;; get compiled value
          (define otype (lookup-typeof ctxt o))
          (define avals
            (for/list ([av a]
                       [at (get-farg-types otype)])
              (cexpr av (update-context! ctxt #:type at))))
          #`(sham-app #,(ast-id-stxid o) #,avals)]
         ;; [(cond (chk thn) ... els) (compile-cond chk thn els)]
         [(var name) (lookup-val ctxt name)]
         [(tvar name) (lookup-type-var ctxt name)]
         [(annot (^ e #:ctxt (update-annot-ctxt ctxt this-ast)) t) e]
         [(where (^ body #:ctxt (update-where-ctxt ctxt this-ast)) ds ...) body]
         [(error msg) 0]
         [(lit i) (integer-literal i ctxt)]
         [(char c) (char-literal c ctxt)]
         [(zero) (zero-literal ctxt)])
  (cseq (sequence -> any)
        ;; [(basic (^ vs) ...) (sham-sequence vs ...)]
        ;; [(enum (^ from) (^ step) (^ to)) (sham-enum from step to)]
        ;; [(str s) (sham-string s)]
        ;; [(comp body [(var val)] ...) (sham-comp body (var val) ...)]
        )
  )

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
      (println (compile-val-defs (list (stx-to-cry-ast d))))))
  (tc bit-id-stx)
  (tc pt1))
