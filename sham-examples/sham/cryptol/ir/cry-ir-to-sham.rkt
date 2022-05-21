#lang racket

(require sham/sam/runtime/identifier
         sham/sam/runtime/ast
         racket/syntax)
(require (for-template (prefix-in ll- sham/llvm/ir)
                       sham/ir))
(require "ast.rkt"
         "sham-ir.rkt")
(provide to-sham)

(define cfuns (box '()))
(define (add-cfun c) (printf "adding cfun: ~a\n" c) (set-box! cfuns (cons c (unbox cfuns))))
(define special-funs (make-hash))
(define special-names (make-hash))
(define gensym-map (make-hash))
(define (new-id! dat)
  (define cnt (hash-ref gensym-map dat #f))
  (define nc (if (false? cnt) 0 (add1 cnt)))
  (hash-set! gensym-map dat nc)
  (string->symbol (format "~a_~a" dat nc)))
(define (to-datum n)
  (cond
    [(ast:id/c n) (syntax->datum (ast-id-stxid n))]
    [else (error 'sham/cry "unknown name ~a" n)]))
(define (get-name n) (to-datum n))

(define (new-name old) (new-id! (to-datum old)))

(define (add-special-pfun name ptypes)
  (define name-datum (syntax->datum (ast-id-stxid name)))

  (define sid (cons name-datum (type->datum ptypes)))
  (define orig (hash-ref special-names sid #f))
  (or orig
      (let ([nn (datum->syntax (ast-id-stxid name) (new-name name-datum))])
        (hash-set! special-names sid nn)
        nn)))

(define (type->datum t)
  (match t
    [(type-bit) 'bit]
    [(type-int) 'int]
    [(type-idx) 'idx]
    [(type-tup ts ...) `(tup ,@(map type->datum ts))]
    [(type-seq d t) `(seq ,d ,(type->datum t))]
    [(type-fun (tps ...) (tvs ...) tto)
     `(-> ,(map type->datum tps) ,(map type->datum tvs) ,(type->datum tto))]
    [(? integer?) t]
    [t `(unkt ,t)]
    [(list ts ...) (map type->datum ts)]))
(define (ctype t)
  (match t
    [(type-bit) sham-bit-type]
    [(type-int) sham-int-type]
    [(type-idx) sham-idx-type]
    [(type-tup ts ...) (sham-tup-type (map ctype ts))]
    [(type-seq d t) (sham-seq-type d (ctype t))]
    [(type-fun (tps ...) (tvs ...) tto)
     (sham-fun-type (map ctype tvs) (ctype tto))]
    [else (error 'sham/cry "unknown type ~a" (type->datum t))]))
;; allocs
(struct abind [name type stype val] #:transparent)
(struct allocat [top rst] #:transparent)

;; -> (listof (or/c abind sham-stmt))
(define (allocate-for-type name type)
  (define sham-type (ctype type))
  (define val (sham-var-ref name))
  (define top (sham-alloca sham-type))
  (flatten
   (list
    (abind name type sham-type top)
    (match type
      [(type-tup ts ...)
       (for/list ([t ts] [i (length ts)])
         (define ias (allocate-for-type (new-name name) t))
         (list ias (sham-store (alloc-ref (car ias)) (sham-tup-idx-ref val i))))]
      [(type-seq d t)
       (define elem-type (ctype t))
       (define ptr-val (sham-arr-alloca elem-type d))
       (define ptr-type (sham-ptr-type elem-type))
       (define ptr-stmt (sham-store ptr-val (sham-seq-arr-ref val)))
       (define dim-stmt (sham-store (sham-dim-val d) (sham-seq-len-ref val)))
       (define upto-stmt (sham-store (sham-dim-val 0) (sham-seq-upto-ref val)))
       (define ias
         (if (needs-ptr-type? t)
             (let* ([idx-sym (new-name 'sqi)]
                    [ias (allocate-for-type (new-name name) t)]
                    [as (sham-store (alloc-ref (car ias)) (sham-seq-idx-ref val (sham-var-ref idx-sym)))])
               (list (sham-iterate-for-seq d idx-sym (bind-allocs ias (list as)))))
             '()))
       (list ptr-stmt dim-stmt upto-stmt ias)]
      [else '()]))))
;; -> allocat/c
(define (create-allocation name type)
  (define as (allocate-for-type name type))
  (allocat (car as) (cdr as)))
(define (get-alloc-let-var a) (list (abind-name a) (abind-val a) (abind-stype a)))
(define (bind-allocs allocats stmts)
  (define-values (binds inits) (partition abind? (append-map allocat-rst allocats)))
  (define all-binds (append (map allocat-top allocats) binds))
  (sham-bind (map get-alloc-let-var all-binds) (sham-stmts (append inits stmts))))
(define (alloc-ref a) (sham-var-ref (abind-name (allocat-top a))))


(define (cons-alloc! val as) (set-box! as (cons val (unbox as))))
(define (append-allocs! vals as) (set-box! as (append vals (unbox as))))
(define (empty-allocs) (box '()))
(define (get-allocs as) (unbox as))
;; closure
(struct closure [name allocs] #:transparent)
(define (create-closure cname allocs) (closure cname allocs))
(define (bind-closure cl stmts)
  (sham-bind (list (list (closure-name cl) #'cl-val #'cl-type)) stmts))
;; env
(struct env [vars clos allocs] #:transparent)
(define (lookup-env var env)
  (define vs (env-vars env))
  (define av (assoc (to-datum var) vs))
  (if av (cdr av) #f))

(define (empty-env) (env '() #f #f))

;; result
(struct cont-result [f] #:transparent)
(struct ref-result [v] #:transparent)
(struct seq-result-i [r i] #:transparent)
(struct tup-result-i [r i] #:transparent)
(struct alloc-result [a] #:transparent)

(define (apply-result r v t)
  (match r
    [(ref-result rv) (sham-store rv v)]
    [(alloc-result a) (sham-store (alloc-ref a) v)]
    [else (error 'todo "apply-result ~a ~a ~a" r v t)]))

(define (apply-var-result r n en) 'todo-var-res)
(define (apply-no-result r v) v)
(define (apply-seq-result r f) 'todo-seq-re)

;; -> list of sham statements assigning the value of e to r
(define (cexpr e r en)
  (match e
    [(expr-bnd t b) (cexpr b r en)]
    [(expr-cnd t (chk thn) ... els)
     (for/fold ([s (cexpr els r en)])
               ([c chk]
                [t thn])
       (cexpr c
              (cont-result
               (λ (cc)
                 (cexpr thn
                        (cont-result (λ (tc) (sham-if cc tc s)))
                        en)))
              en))]
    [(expr-app t op (args ...))
     (match op
       [(expr-fvar t d) #'todo-app-fvar]
       [(expr-pvar t n) #'todo-app-pvar]
       [else (error 'sham/cry "unknown op for app ~a" op)])]
    [(expr-tup t vals ...)
     (for/list ([v vals]
                [i (length vals)])
       (cexpr v (tup-result-i r i) en))]
    [(expr-lit t i) (apply-result r (sham-int-literal i t) t)]
    [(expr-seq-val t vals ...)
     (match-define (type-seq d ts) t)
     (for/list ([v vals]
                [i d])
       (cexpr v (seq-result-i r i) en))]
    [(expr-seq-enm t f s e)
     (apply-seq-result r (λ (i) `(+ ,f (* ,i ,s))))]
    [(expr-seq-laz t idx val)
     (cond [(zero? (type-seq-dim t)) (sham-void)]
           [else (error 'todo-seq-laz)])]
    [(expr-tup-idx t v i)
     (cexpr v (cont-result (λ (v) (apply-result r (sham-tup-idx v i) t))) en)]
    [(expr-seq-idx t v i)
     (cexpr v (cont-result (λ (v) (apply-result r (sham-seq-idx v i) t))) en)]
    [(expr-seq-len t v)
     (cexpr v (cont-result (λ (v) (apply-result r (sham-seq-len v) t))) en)]
    [(expr-bnv t n) (apply-var-result r n en)]
    [(expr-lid t n) (apply-var-result r n en)]
    ;; [(expr-fvar t d) (todo 'fvar t d e)]
    ;; [(expr-pvar t n) (todo 'pvar t n e)]
    [(expr-frg t i) (apply-result r (sham-farg i) t)]
    [(expr-err t s) (apply-no-result r (sham-err s))]
    [(def-val n t v) (apply-var-result r n en)]
    [else (error 'sham/cryptol "unknown expr: ~a" e)]))

(define (cfun d e)
  (match-define (def-cfun fname ftype (expr-bnd btype (def-val vnames vtypes vvals) ... fbody)) d)

  (define nn (add-special-pfun fname ftype))
  (define old (hash-ref special-funs nn #f))
  (define (comp!)
    ;; closure
    (define cname (new-name 'clos))

    ;; binds
    (define allocates (map create-allocation vnames vtypes))
    (define cl (create-closure cname allocates))
    (define as (empty-allocs))
    (append-allocs! allocates as)
    (define new-e (env (map cons (map get-name vnames) allocates) cl as))

    (define vvals-s (map (curryr cexpr new-e) vvals (map alloc-result allocates)))

    (define ret-res (ref-result (sham-farg (length vtypes))))
    (define fbody-s (cexpr fbody ret-res new-e))

    (define f-s (bind-allocs (append allocates (get-allocs as)) (bind-closure cl (append vvals-s (list fbody-s) (sham-fun-final)))))
    (define f-v (sham-fun nn (ctype ftype) f-s))
    (add-cfun f-v)
    (hash-set! special-funs nn f-v))
  (unless old (comp!))
  nn)

(define (cdef d e)
  (match d
    ;; [(def-val n t v) `(val ,n ,(ctype t) ,(cexpr v))]
    [(def-cfun n t b) (cfun d e)]
    [(def-pfun n (tps ...) t)
     ;; (define nn (pfun d))
     `(pfun ,n ,(map ctype tps))
     ]))
(define (ctest t)
  ;; (printf "ctext: ~a" t)
  (define (test-compare typ v1 v2)
    (match typ
      [(type-bit) (sham-cmp-int (sham-val v1) (sham-val v2))]
      [(or (type-idx) (type-int) (type-seq _ (type-bit))) (sham-cmp-int (sham-val v1) (sham-val v2))]
      [(type-seq d t) (sham-iterate-for-seq d (λ (i) (sham-cmp-int (sham-seq-idx v1 i) (sham-seq-idx v2 i))))] ;; todo fold with a bit
      [(type-tup ts ...) (sham-andn (for/list ([t ts] [i (length ts)]) (test-compare t (sham-tup-idx v1 i) (sham-tup-idx v2 i))))]))
  (match t
    [(def-tst name typ v1 v2)
     (define v1a (create-allocation (new-name name) typ))
     (define v2a (create-allocation (new-name name) typ))
     (define as (empty-allocs))
     (define en (env '() #f as))
     (define v1-s (cexpr v1 (alloc-result v1a) en))
     (define v2-s (cexpr v2 (alloc-result v2a) en))
     (define t-s (bind-allocs (append (list v1a v2a) (get-allocs as))
                              (list v1-s v2-s (test-compare typ (alloc-ref v1a) (alloc-ref v2a)) (sham-test-final))))
     (define nn (get-name name))
     (define t-v (sham-test nn t-s))
     (cons nn t-v)]))

(define (to-sham test-specs)
  (match-define (cons tests specs) test-specs)
  (printf "to-sham: \n")
  (map (compose pretty-print syntax->datum cdr ctest) tests)
  ;; (pretty-print (map cdef specs))
  (pretty-print (unbox cfuns))
  (pretty-print special-names)
  (pretty-print special-funs)
  #`(42))
