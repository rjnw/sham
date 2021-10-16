#lang racket

(require "utils.rkt")
(require sham/sam/runtime)

(provide (all-defined-out))

(struct env-var [name (val #:mutable) (type #:mutable)] #:transparent
  ;; #:methods gen:custom-write
  ;; [(define (write-proc v port mode) (display (env-var-name v) port))]
  )

(struct env-lazy-var env-var [] #:transparent) ;; val is wrapped in a function which takes pargs vargs and ctxt to compile
(struct env-primitive-var env-var [] #:transparent)     ;; primitive value which should compile an app primitively
(struct env-special-var env-var [oname otype pargs] #:transparent) ;; lazy compile returns a specialized value for specific pargs and gensym'd name

(define (get-from-env-vars env-vars get)
  (match env-vars
    ['() #f]
    [(cons fst rst) (or (get fst) (get-from-env-vars rst get))]))

(define (lookup-env-vars evs name)
  (define (is-val? v)
    (id-free=? (env-var-name v) name))
  (filter is-val? evs))

(define ((lookup-in-env f getter) c/e name)
  (define env (cond
                [(cc? c/e) (cc-env c/e)]
                [(env? c/e) c/e]
                [else (error 'sham/cryptol "unknown ctxt/env ~a" c/e)]))
  ;; (debug (printf "looking-in-env: ~a ~a\n" f name) (print-env env))
  (get-from-env-vars (lookup-env-vars (f env) name) getter))
(define (combine-env-vars vars1 vars2)
  (define (combine from into)
    (match-define (env-var fname fval ftype) from)
    (match-define (env-var tname tval ttype) into)
    (set-env-var-val! into (or fval tval))
    (set-env-var-val! into (or ftype ttype))
    into)
  (match vars1
    ['() vars2]
    [(cons fst rst)
     (cons
      (cond [(lookup-env-vars (env-var-name fst) rst) => (combine fst)]
            [else fst])
      (combine-env-vars rst vars2))]))

(define (print-ev ev)
  (match ev
    [(env-lazy-var name val type)
     (printf "   ~a:~a" name type)]
    [(env-special-var name val type oname otype pargs)
     (printf "   ~a<~a:~a>:~a=~a" name oname pargs type val)]
    [(env-var name val type)
     (printf "   ~a:~a=~a" name type val)] )
  ev)

(define (print-evs evs) (for [(ev evs)] (print-ev ev) (newline)) evs)

;; type stores name type for val and kind for type in bind
(struct env [type val] #:prefab)
(define (print-env e)
  (match-define (env ts vs) e)
  (printf "  type:\n") (print-evs ts)
  (printf "  vals:\n") (print-evs vs)
  e)
(define (empty-env) (env '() '()))
(define (update-env c/e #:type (types '()) #:val (vals '()) #:combine-with (cw append))
  (define (doe e)
    (match-define (env ot ov) e)
    (env (cw (if (env-var? types) (list types) types) ot)
         (cw (if (env-var? vals) (list vals) vals) ov)))
  (cond [(cc? c/e) (update-context! c/e #:env (doe (cc-env c/e)))]
        [(env? c/e) (doe c/e)]
        [else (doe (env '() '()))]))

;; context keeps track of current type, poly type vars currently active, result value and lifts
(struct cc [type env pvars res lifts] #:prefab)
(define (print-cc c)
  (match-define (cc t env pvars res lifts) c)
  (printf "ctxt:\n type: ~a\n pvars: ~a\n res: ~a\n #lifts: ~a\n" t pvars res (length (unbox lifts)))
  (printf " env:\n")
  (print-env env)
  c)
(define (empty-context) (cc #f (empty-env) '() #f (box '())))
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

(define (add-lifts! c . lfs)
  (define lifts (flatten lfs))
  (set-box! (cc-lifts c) (append (unbox (cc-lifts c)) lifts))
  c)

(define lookup-env-val (lookup-in-env env-val identity))
(define lookup-val (lookup-in-env env-val env-var-val))
(define lookup-typeof (lookup-in-env env-val env-var-type))
(define lookup-type (lookup-in-env env-type env-var-val))
(define lookup-kind (lookup-in-env env-type env-var-type))
