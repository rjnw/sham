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

(struct env [type typeof val] #:prefab)
(define (print-env e)
  (match-define (env t to vl) e)
  (define (print-as ls)
    (for ([p ls])
      (printf "   ~a: ~a\n" (car p) (cdr p))))
  (printf "  type:\n") (print-as t)
  (printf "  typeof:\n") (print-as to)
  (printf "  vals:\n") (print-as vl))

(define (update-env oe #:type (type '()) #:typeof (typeof '()) #:val (val '()))
  (match-define (env ot oto ov) (or oe (env '() '() '())))
  (env (append type ot) (append typeof oto) (append val ov)))

(struct cc [type env pvars res lifts] #:prefab)

(define (print-cc c)
  (match-define (cc t env pvars res lifts) c)
  (printf "ctxt:\n type: ~a\n pvars: ~a\n res: ~a\n #lifts: ~a\n" t pvars res (length (unbox lifts)))
  (printf " env:\n")
  (print-env env)
  c)

(define (update-context! (from #f)
                         #:type (type #f)
                         #:env (env #f)
                         #:lifts (lifts '())
                         #:pvars (pvars #f)
                         #:res (res #f))
  (cond
    [(cc? from)
     (match-define (cc t oe op os ol) from)
     (unless (empty? lifts) (set-box! ol (append lifts (unbox ol))))
     (cc (or type t) (or env oe) (or pvars op) (or res os) ol)]
    [else (cc type env pvars res (if (box? lifts) lifts (box lifts)))]))
(define ((lookup-in-env f) ctxt name)
  (printf "looking-in-env: ~a ~a\n" f name)
  (print-cc ctxt)
  (define -env (f (cc-env ctxt)))
  (define pair (assoc name -env id-free=?))
  (and pair (cdr pair)))
(define lookup-typeof (lookup-in-env env-typeof))
(define lookup-val (lookup-in-env env-val))
(define lookup-type (lookup-in-env env-type))
(define lookup-type-var (lookup-in-env env-val))

(define (update-def-val-ctxt ctxt ast)
  (match-define (def-val name val) ast)
  (define-values (uptype pvars cs) (unwrap-poly (lookup-typeof ctxt name)))
  ;; TODO use constraints
  (update-context! ctxt #:type uptype #:pvars pvars))

;; TODO
(define (sham-tuple-index val i) #`(ll-op-gep #,val 0 #,i))
(define (sham-sequence-index val i) #`(ll-op-gep #,val 1 #,i))

(define (update-bind-ctxt ctxt ast)
  (printf "updating-bind-ctxt: ~a\n" ast)
  (print-cc ctxt)
  (define (pat-bind pat-ast val typ)
    (match pat-ast
      [(pat-var name) (list (list name val typ))]
      [(pat-tuple ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)]
                         [t (type-tuple-ts typ)])
                (pat-bind p (sham-tuple-index val i) t)))]
      [(pat-sequence ps ...)
       (apply append
              (for/list ([p ps]
                         [i (length ps)])
                (pat-bind p (sham-sequence-index val i) (type-sequence-t typ))))]))
  (define ctype (cc-type ctxt))
  (define all-farg-types (get-farg-types ctype))
  (match-define (list fargs ... res) (build-list (length all-farg-types) (λ (i) #`(ll-val-param #,i))))

  (printf "binds: ~a, ~a\n" (expr-bind-ps ast) fargs)
  (define pat-binds (append-map pat-bind (expr-bind-ps ast) fargs (drop-right all-farg-types 1)))
  (printf "updating-for-bind: pat-binds: ~a\n" pat-binds)
  (print-cc (update-context! ctxt
                    #:type (last all-farg-types)
                    #:res res
                    #:env (update-env (cc-env ctxt)
                                      #:val (map (curryr drop-right 1) pat-binds)
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
    (cry-def-to-sham-stx (car vt) (update-context! new-ctxt #:type (def-typeof-val (cdr vt))))))

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

(define-transform (cry-def-to-sham-stx (ctxt #f))
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        [(val (^ name) (^ body #:ctxt (update-def-val-ctxt ctxt this-ast)))
         #`(make-def-function #,name #,(sham-def-type ctxt this-ast) #,body)])
  (cexpr (expr -> any)
         [(bind (p ...) (^ res #:ctxt (update-bind-ctxt ctxt this-ast))) res]
         ;; [(app o (i ...) a ...) (sham:expr:op (sham-op o ctxt) (sham-args i a))]
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
  (define bit-id-any #`(def [id : {a} a -> a] [id a = a]))
  (define proj1 #`(def [p1 : #(bit bit) -> bit]
                    [p1 #(b1 b2) = b1]))
  ;; (define bit-id-cast (stx-to-cry-ast bit-id-stx))
  ;; (println bit-id-cast)

  (define (tc d)
    (with-handlers ([exn:fail? (λ (e) (displayln e) ((error-display-handler) "error-compiling" e))])
      (println (compile-val-defs (list (stx-to-cry-ast d))))))
  (tc bit-id-stx)
  (tc proj1))
