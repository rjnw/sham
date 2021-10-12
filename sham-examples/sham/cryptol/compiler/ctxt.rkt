#lang racket

(require sham/sam/runtime)

(provide (all-defined-out))

(struct env-var [name (val #:mutable) (type #:mutable)] #:transparent)
(struct env-svar env-var [oname otype pargs] #:transparent)

(define ((find-in-env-vars get) env-vars)
  (match env-vars
    ['() #f]
    [(cons fst rst) (or (get fst) ((find-in-env-vars get) rst))]))
(define (print-ev ev)
  (match ev
    [(env-var name val type)
     (printf "~a:~a=~a" name type val)]
    [(env-svar name val type oname otype pargs)
     (printf "~a<~a:~a>:~a=~a" name oname pargs val type oname)])
  ev)
(define (print-evs evs) (for [(ev evs)] (print-ev ev) (newline)) evs)

;; type stores name type for val and kind for type in bind
(struct env [type val] #:prefab)
(define (print-env e)
  (match-define (env ts vs) e)
  (printf "  type:\n") (print-evs ts)
  (printf "  vals:\n") (print-evs vs)
  e)

(define (update-env oe #:type (types '()) #:val (vals '()))
  (match-define (env ot ov) (or oe (env '() '())))
  (env (append types ot) (append vals ov)))
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

(define (add-lifts! c . lfs)
  (define lifts (flatten lfs))
  (set-box! (cc-lifts c) (append (unbox (cc-lifts c)) lifts))
  c)

(define ((lookup-in-env f) c/e name)
  (printf "looking-in-env: ~a ~a\n" f name)
  (define env (cond
                [(cc? c/e) (cc-env c/e)]
                [(env? c/e) c/e]
                [else (error 'sham/cryptol "unknown ctxt/env ~a" c/e)]))
  (print-env env)
  (define -env (f env))
  (define (is-val? v)
    (id-free=? (env-var-name v) name))
  (filter is-val? -env))


(define lookup-val (compose (find-in-env-vars env-var-val) (lookup-in-env env-val)))
(define lookup-typeof (compose (find-in-env-vars env-var-type) (lookup-in-env env-val)))
(define lookup-type (compose (find-in-env-vars env-var-val) (lookup-in-env env-type)))
(define lookup-kind (compose (find-in-env-vars env-var-type) (lookup-in-env env-type)))
