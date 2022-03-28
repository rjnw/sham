#lang racket

(require "ast.rkt")
(require sham/sam/runtime
         sham/sam/pretty)

(provide (all-defined-out))

(struct env-var [name val] #:transparent)
(struct env-var-prim-typeof env-var [] #:transparent)
(struct env-var-prim-type env-var [] #:transparent)

;; also stores type
(struct env-var-wt env-var [type] #:transparent)
(struct env-var-prelude env-var-wt [] #:transparent)
(struct env-var-prog env-var-wt [] #:transparent)

;; context keeps track of current type, poly type vars currently active, result value and lifts
(struct cc [origs typs vals typofs]
  #:methods gen:custom-write
 [(define (write-proc val port mode)
     (fprintf port "<env>")
     #;(parameterize ([current-output-port port])
       (print-env val)))] )

(define (empty-ctxt) (cc '() '() '() '()))

(define (update-ctxt c #:vals (vals '()) #:types (types '()) #:typeofs (typeofs '()) #:orig (origs '()))
  ;; (printf "update-ctxt:") (pretty-ctxt c)
  ;; (and (not (empty? vals)) (printf "vals: ~a\n" vals))
  ;; (and (not (empty? types)) (printf "types: ~a\n" types))
  ;; (and (not (empty? typeofs)) (printf "typeofs: ~a\n" typeofs))
  (unless (andmap env-var? types) (error "update-ctxt: unknown types: ~a\n") types)
  (define (cmb a rst)
    (define (valid? a) (or (env-var? a) (def? a)))
    (unless (or (valid? a) (andmap valid? a))
      (error 'cry/ir/ctxt "unknown update: ~a" a))
    (if (list? a) (append a rst) (cons a rst)))
  (match-define (cc os ts vs to) c)
  (cc (cmb origs os) (cmb types ts) (cmb vals vs) (cmb typeofs to)))
(define (pretty-ctxt c)
  (match-define (cc os types vals typeofs) c)
  (printf "ctxt: \n")
  (and (not (empty? vals)) (printf "\tvals: ~a\n" vals))
  (and (not (empty? types)) (printf "\ttypes: ~a\n" types))
  (and (not (empty? typeofs)) (printf "\ttypeofs: ~a\n" typeofs))
  (newline))

(define (get-name v)
  (cond [(env-var? v) (env-var-name v)]
        [(def-val? v) (def-val-name v)]
        [else (error 'cry/ir "ctxt unknown value ~a" v)]))
(define (lookup-env-vars evs name)
  (define (is-val? v) (id-datum=? (get-name v) name))
  (filter is-val? evs))

(define (ctxt-lookup-val c name)
  (define vals (lookup-env-vars (cc-vals c) name))
  (or (get-first vals)
      (error 'cry/ir "ctxt-lookup-val: ~a" name)
      (begin (printf "warn:ctxt-lookup-val: ~a" name) #f)))
(define (ctxt-has-val? c name) (not (empty? (lookup-env-vars (cc-vals c) name))))

(define (get-first evs (f identity))
  (and (cons? evs) (car evs) (f (car evs))))

;; -> (values org-ast org-type)
(define (ctxt-lookup-orig c name)
  (define orig (get-first (lookup-env-vars (cc-origs c) name)))
  (cond [(env-var-wt? orig) (values (env-var-val orig) (env-var-wt-type orig))]
        [(env-var-prim-typeof? orig) (values 'primitive (env-var-val orig))]
        [else (error 'cry/ir "ctxt: lookup-orig: ~a \n" name)]))
;; returns original type
(define (ctxt-lookup-typeof c name)
  (printf "ctxt: lookup-typeof: ~a \n" name)
  (define typofs (lookup-env-vars (cc-typofs c) name))
  (define val (ctxt-lookup-val c name))
  ;; (printf "ctxt: lookup-typeof: ~a \n" origs)
  (or (get-first typofs env-var-val)
      (expr-type val)
      (error 'cry/ir "ctxt-lookup-typeof ~a" name)))
(define (ctxt-lookup-type-var c name)
  (printf "ctxt: lookup-type-var: ~a \n" name)
  (define opts (lookup-env-vars (cc-typs c) name))
  (or (get-first opts env-var-val)
      (begin
        (printf "warn:ctxt-lookup-type-var ~a ~a\n" name (cc-typs c))
        #f)))
