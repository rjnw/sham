#lang racket

(require "utils.rkt")
(require sham/sam/runtime)

(provide (all-defined-out))

(struct env-var [name val] #:transparent)

(struct env-lazy-var env-var [ast] #:transparent) ;; val is wrapped in a function which takes pargs vargs and ctxt to compile
(struct env-primitive-var env-var [] #:transparent)     ;; primitive value which should compile an app primitively
(struct env-prelude-var env-var [] #:transparent)     ;; prelude value
(struct env-special-var env-var [type oname otype pargs] #:transparent) ;; lazy compile returns a specialized value for specific pargs and gensym'd name

(define (maybe-first-env-vars env-vars (getter env-var-val))
  (match env-vars
    ['() #f]
    [(cons fst rst) (getter fst)]))

(define (lookup-env-vars evs name)
  (define (is-val? v)
    (id-datum=? (env-var-name v) name))
  (filter is-val? evs))

(define ((lookup-in-env f (getter env-var-val)) c/e name)
  (define env (cond
                [(cc? c/e) (cc-env c/e)]
                [(env? c/e) c/e]
                [else (error 'sham/cryptol "unknown ctxt/env ~a" c/e)]))
  ;; (debug (printf "looking-in-env: ~a ~a\n" f name) (print-env env))
  (maybe-first-env-vars (lookup-env-vars (f env) name) getter))

(define (print-ev ev)
  (match ev
    [(env-primitive-var n v) (void)]
    [(env-prelude-var n v) (void)]
    [(env-special-var name val type oname otype pargs)
     (printf "~a<~a:~a>:~a, " name oname (map pretty-cry pargs) (pretty-cry otype))]
    [(env-lazy-var name val ast) (printf "~a%, " name)]
    [(env-var name val) (printf "~a=~a$\n" name (pretty-cry val))]
    ;; [(env-primitive-var name val) (printf "  ~a:~a\n" name (pretty-cry type))]
    ;; [(env-var name #f)
    ;;  (printf "  ~a:~a\n" name (pretty-cry type))]
    ;; [(env-var name #f) (printf "  ~a?" name)]
    )
  ev)

(define (print-evs evs) (for [(ev evs)] (print-ev ev)) evs)

;; type stores name type for val and kind for type in bind
(struct env [val typeof type tvar]
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (fprintf port "<env>")
     #;(parameterize ([current-output-port port])
       (print-env val)))])

(define (print-env e)
  (match-define (env vs tos ts tvs) e)
  (printf "  vals:\n") (print-evs vs)
  (printf "  typofs:\n") (print-evs tos)
  (printf "  type:\n") (print-evs ts)
  (printf "  tvars:\n") (print-evs tvs)
  e)

(define lookup-val-env (lookup-in-env env-val identity))
(define lookup-val (lookup-in-env env-val))
(define lookup-typeof (lookup-in-env env-typeof))
(define lookup-type (lookup-in-env env-type))
(define lookup-tvar (lookup-in-env env-tvar))
(define (empty-env) (env '() '() '() '()))
(define (update-env c/e
                    #:val (vals '())
                    #:typeof (typeofs '())
                    #:type (types '())
                    #:tvar (tvars '()))
  ;; (printf "update-env: ~a ~a ~a ~a\n" vals typeofs types tvars)
  (define (combine news old)
    (append (if (env-var? news) (list news) news) old))
  (define (doe e)
    (match-define (env ovals otypeofs otypes otvars) e)
    (env (combine vals ovals)
         (combine typeofs otypeofs)
         (combine types otypes)
         (combine tvars otvars)))
  (cond [(cc? c/e) (update-context! c/e #:env (doe (cc-env c/e)))]
        [(env? c/e) (doe c/e)]
        [else (doe (empty-env))]))

;; context keeps track of current type, poly type vars currently active, result value and lifts
(struct cc [type env pvars res cctxt lifts]
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (fprintf port "<ctxt>")
     #;(parameterize ([current-output-port port])
       (print-cc val)))])
(define (print-cc c)
  (match-define (cc t env pvars res icc lifts) c)
  (printf "ctxt:\n type: ~a\n pvars: ~a\n res: ~a\n #lifts: ~a\n" t pvars res (length (unbox lifts)))
  (printf " env:\n")
  (print-env env)
  c)
(define (empty-context) (cc #f (empty-env) '() #f #f (box '())))
(define (update-context! (from #f)
                         #:type (type #f)
                         #:env (env #f)
                         #:lifts (lifts '())
                         #:pvars (pvars #f)
                         #:res (res #f)
                         #:cc (cctxt #f))
  (cond
    [(cc? from)
     (match-define (cc t oe op os oc ol) from)
     (unless (empty? lifts) (set-box! ol (append lifts (unbox ol))))
     (cc (or type t) (or env oe) (or pvars op) (or res os) (or oc cctxt) ol)]
    [else (cc type env pvars res cctxt (if (box? lifts) lifts (box lifts)))]))

(define (add-lifts! c . lfs)
  (define lifts (flatten lfs))
  (set-box! (cc-lifts c) (append (unbox (cc-lifts c)) lifts))
  c)
