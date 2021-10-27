#lang racket
(require (for-syntax syntax/parse)
         racket/trace)
(require "../ast.rkt"
         sham/sam/transform
         sham/sam/rkt
         sham/sam/runtime/identifier)

(provide (all-defined-out))

(define (remove-gen-defs ds)
  (flatten
   (let loop ([defs ds])
     (cond [(list? defs) (map loop defs)]
           [(def-gen? defs) (loop (def-gen-ds defs))]
           [else defs]))))

(define (split-defs defs)
  (define val-defs (filter def-val? defs))
  (define type-defs (filter def-type? defs))
  (define typeof-defs (filter def-typeof? defs))
  (define test-defs (filter def-test? defs))
  (values val-defs type-defs typeof-defs test-defs))

(define (find-typeof name tdefs)
  (define (is-type? tdef)
    (match-define (def-typeof tnames ... t) tdef)
    (ormap (λ (tname) (free-identifier=? (ast-id-stxid name) (ast-id-stxid tname))) tnames))
  (findf is-type? tdefs))

(define (combine-val&typeof vdefs tdefs)
  (define ctvs
    (for/fold ([res '()]
               #:result (reverse res))
              [(d vdefs)]
      (match-define (def-val name e) d)
      (define to (find-typeof name tdefs))
      (cons (def-typed-val name (if to (def-typeof-val to) (type-unknown)) e) res)))
  (define (remove-used tdef)
    (match tdef
      [(def-typeof names ... val)
       (define (has-val n)
         (member n ctvs (λ (n1 ctv) (free-identifier=? (ast-id-stxid n1) (ast-id-stxid (def-typed-val-name ctv))))))
       (define new-names (filter-not has-val names))
       (if (empty? new-names) #f (make-def-typeof new-names val))]))
  (values ctvs (filter-map remove-used tdefs)))

(define-transform (pretty-cry-ast)
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        [(gen (^ ds) ...) ds]
        [(val n (^ v)) `(,n = ,v)]
        [(typeof ns ... (^ v)) `(,@ns : ,v)]
        [(type n (^ t)) `(type ,n : ,t)]
        [(typed-val n (^ t) (^ v)) `(,n : ,t = ,v)]
        [(combined ((^ ts) ...) ((^ tos) ...) ((^ vs) ...) ((^ t) ...)) `(,@ts ,@tos ,@vs ,@t)]
        [(test n (^ v1) (^ v2)) `(test ,n : ,v1 = ,v2)])
  (cpat (pat -> any)
        [(var n) n]
        [(tuple (^ ps) ...) `(tuple ,@ps)]
        [(sequence (^ ps) ...) (apply vector ps)])
  (cexpr (expr -> any)
         [(bind ((^ ps) ...) (^ b)) `(,@ps = ,b)]
         [(app (^ o) ((^ ia) ...) (^ a) ...) `(,o {,@ia} ,@a)]
         [(cond ((^ c) (^ t)) ... (^ e)) `(cond ,@(map cons c t) ,e)]
         [(var n) n]
         [(tvar n) `',n]
         [(annot (^ e) (^ t)) `(,e : ,t)]
         [(where (^ b)) b]
         [(where (^ b) (^ ds) ...) `(where ,b ,ds)]
         [(error msg) `(error ,msg)]
         [(lit i) i]
         [(char c) c]
         [(tuple (^ vs) ...) `($ ,@vs)])
  (cseq (sequence -> any)
        [(basic (^ vs) ...) (apply vector vs)]
        [(enum (^ f) (^ s) (^ t)) `(,f .. ,s ,t)]
        [(str s) `(str ,s)]
        [(comp (^ body) (((^ v) (^ l))) ...)
         `[,body \| ,@(map cons v l)]])
  (ctyp (type -> any)
        [bit 'bit]
        [integer 'int]
        [unknown 'unk]
        [(sequence (^ d) (^ t)) (vector d t)]
        [(tuple (^ ts) ...) `(tuple ,@ts)]
        [(var n) n]
        [(poly (vars ...) (^ t)) `(,vars : ,t)]
        [(constraint (cs ...) (^ t)) `(,@(map syntax->datum cs) => ,t)]
        [(func (^ f) (^ t)) `(,f -> ,t)]
        [t `(unk ,t)])
  (cdim (dim -> any)
        [(int v) v]
        [(app op (^ args) ...) `(,(syntax->datum op) ,@args)]
        [(var n) `(dim ,n)]
        [d `(unk-dim ,d)]))

(define (pretty-cry v) (if (struct-cry-ast? v) (pretty-cry-ast v) v))
(define (new-name-def old) (ast-id-gen old))
(define (lookup name env-vars)
  (define (is-ev? name ev) (id-free=? name ev))
  (cond
    [(assoc name env-vars is-ev?) => (λ (ar) (id-create-ref (cdr ar)))]
    [else name]))
(define var-pair cons)

(require racket/trace)
(define (do-pats pats env)
  (define (rec pat)
    (match pat
      [(pat-var n)
       (define nn (new-name-def n))
       (values (pat-var nn) (list (var-pair n nn)))]
      [(pat-tuple ps ...)
       (for/fold ([pps '()]
                  [nns '()]
                  #:result (values (make-pat-tuple pps) nns))
                 ([p ps])
         (define-values (p^ ns) (rec p))
         (values (append pps (list p^)) (append nns ns)))]
      [(pat-sequence ps ...)
       (for/fold ([pps '()]
                  [nns '()]
                  #:result (values (make-pat-sequence pps) nns))
                 ([p ps])
         (define-values (p^ ns) (rec p))
         (values (append pps (list p^)) (append nns ns)))]))
  (for/fold ([ps '()]
             [nns '()])
            ([p pats])
         (define-values (p^ ns) (rec p))
     (values (append ps (list p^)) (append nns ns))))

(define-transform (gensym-names-ast (val-env '()) (type-env '()))
  (cry-ast -> cry-ast)
  (cdef (def -> def)
        ;; [(gen (^ ds) ...) (make gen ds)]
        ;; [(val n v) (let ([nd (new-name-def n)])
        ;;              (make val nd (cexpr v (cons (var-pair n nd) val-env) type-env)))]
        [(typeof ns ... (^ v)) (make typeof ns v)]
        [(type n (^ t)) (make type n t)]
        [(test n v1 v2) this-ast])
  (cexpr (expr -> expr)
         [(bind (ps ...) b) (let-values ([(nps nns) (do-pats ps val-env)])
                              (make bind nps (cexpr b (append nns val-env) type-env)))]
         [(app (^ o) ((^ ia) ...) (^ a) ...) (make app o ia a)]
         [(cond ((^ c) (^ t)) ... (^ e)) (make cond c t e)]
         [(var n) (make var (lookup n val-env))]
         [(tvar n) (make tvar (lookup n type-env))]
         [(annot (^ e) (^ t)) (make annot e t)]
         [(where (^ b) ds ...)
          (let-values ([(new-ds new-val-env new-type-env) (gensym-names-defs ds val-env type-env)])
            (make where b ;; (cexpr b new-val-env new-type-env)
                  (match new-ds
                    [(def-combined () () () ()) (list)]
                    [else (list new-ds)])))]
         [(error msg) (make error msg)]
         [(lit i) this-ast]
         [(char c) this-ast]
         [(tuple (^ vs) ...) (make tuple vs)])
  (cseq (sequence -> sequence)
        [(basic (^ vs) ...) (make basic vs)]
        [(enum (^ f) (^ s) (^ t)) (make enum f s t)]
        [(str s) this-ast]
        [(comp body ((v l)) ...)
         (let-values ([(nvs nns) (do-pats v val-env)])
           (make comp (cexpr body (append nns val-env) type-env) (map list nvs) (map list l)))])
  (ctype (type -> type)
         [bit this-ast]
         [integer this-ast]
         [unknown this-ast]
         [(sequence (^ d) (^ t)) (make sequence d t)]
         [(tuple (^ ts) ...) (make tuple ts)]
         [(var n) (make var (lookup n type-env))]
         [(poly (vars ...) t)
          (let ([nvs (map new-name-def vars)])
            (make poly nvs (ctype t val-env (append (map var-pair vars nvs) type-env))))]
         [(constraint (cs ...) (^ t)) (make constraint cs t)] ;; todo rename in cs
         [(func (^ f) (^ t)) (make func f t)])
  (cdim (dim -> dim)
        [(int v) this-ast]
        [(app op (^ args) ...) (make app op args)]
        [(var n) (make var (lookup n type-env))]))

(define (gensym-names-defs defs (val-env '()) (type-env '()))
  (define (do-tv-def vt-def val-env type-env)
    (define (do-type type type-env)
      ;; to keep track of top level poly var names for function body
      (match type
        [(type-poly (vars ...) t)
         (let* ([nvs (map new-name-def vars)]
                [nte (append (map var-pair vars nvs) type-env)])
           (values (make-type-poly nvs (gensym-names-ast t val-env nte)) nte))]
        [else (values (gensym-names-ast type val-env type-env) type-env)]))
    (define-values (new-def-val nvn new-type-env)
      (match vt-def
        [(def-typed-val name type val)
         (define-values (new-type nte) (do-type type type-env))
         (define nn (new-name-def name))
         (define vnn (var-pair name nn))
         (values (make-def-typed-val name new-type (gensym-names-ast val val-env #;(cons vnn val-env) nte))
                 vnn
                 nte)]))
    (values new-def-val (cons nvn val-env) type-env))
  (define (do-defs defs val-env type-env)
    (define-values (val-defs type-defs typeof-defs test-defs) (split-defs defs))
    (define-values (ctvs rest-typeofs) (combine-val&typeof val-defs typeof-defs))
    (define-values (new-ctvs new-val-env new-type-env)
      (for/fold ([nctvs '()]
                 [ve val-env]
                 [te type-env])
                ([ctv ctvs])
        (define-values (nctv nve nte) (do-tv-def ctv ve te))
        (values (append nctvs (list nctv)) nve nte)))
    (values
     (make-def-combined
      (map (curryr gensym-names-ast new-val-env new-type-env) type-defs)
      (map (curryr gensym-names-ast new-val-env new-type-env) rest-typeofs)
      new-ctvs
      (map (curryr gensym-names-ast new-val-env new-type-env) test-defs))
     new-val-env
     new-type-env))
  (do-defs (remove-gen-defs defs) val-env type-env))

(module+ test
  (require "stx-to-cry-ast.rkt"
           "../prelude/cryptol.rkt")
  ;; (gensym-names-defs
  ;;  (append (remove-gen-defs (stx-to-cry-ast primitive-typeofs-stx))
  ;;          (remove-gen-defs (stx-to-cry-ast prelude-defs-stx))
  ;;          ))
  (define-values (new-ast val-env type-env)
    (gensym-names-defs
     (stx-to-cry-ast #`(def
                         [sha1 : {n} (<= (width (* 8 n)) 64) => [n [8]] -> [160]]
                         [sha1 msg = (sha1^ pmsg)
                               [pmsg : [(/^ (+ (* n 8) 65) 512) [512]]]
                               [pmsg = (pad (join msg))]]))))
  (pretty-print (pretty-cry new-ast)))
