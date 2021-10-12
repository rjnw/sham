#lang racket

(require sham/sam/runtime)

(provide (all-defined-out))

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
(define lookup-type-var (lookup-in-env env-type))
