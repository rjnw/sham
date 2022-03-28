#lang racket

(require sham/sam/runtime/identifier
         sham/sam/runtime/ast
         racket/syntax)
(require (for-template (prefix-in ll- sham/llvm/ir)
                       sham/ir))

(require "compiler.rkt"
         "ctxt.rkt"
         "../ast.rkt"
         "utils.rkt"
         "debug.rkt"
         "types.rkt")

(provide sham-compiler
         require-syntax
         extra-syntax
         primitive-defs)


(struct res [])
(struct ref res [v] #:transparent)                    ;; stores a pointer to the actual value
(struct laz res [] #:transparent)
(struct laz-seq laz [n t fn ctxt] #:transparent)                     ;; lazy sequence, needs special care
;; (struct laz-crn laz [seq idx] #:transparent)
(struct laz-ref laz [seq idx] #:transparent)
(struct laz-slc laz [seq beg] #:transparent)

(struct ret ref [])                     ;; special for function return value, it is always a reference
(struct stm res [stmts res] #:transparent)

;; alloca triplet for let binds
(struct allocat [name val type] #:transparent)

;; internal context
(struct ic [allocats allocatives closure] #:transparent)
(struct closure [name ctxt types local-evs] #:transparent)

(define (unwrap-result r)
  (match r
    [(stm stmts res) (define-values (ss r^) (unwrap-result res))
                     (values (append stmts ss) r^)]
    [else (values '() r)]))
(define (load-if-ref r)
  (match r
    [(ref v) #`(op-load #,(load-if-ref v))]
    [else r]))
(define (load-if-needed v t)
  (if (use-ptr-type? t)
      (match v
        [(ref p) p]
        [(ret p) p]
        [else (error "load-if-needed: needs pointer type: ~a ~a" v t)])
      (load-if-ref v)))
(define (unref-if-ptr v t) (if (use-ptr-type? t) (ref-v v) v))
(define (ref-if-ptr v t) (if (use-ptr-type? t) (ref v) v))
(define (maybe-stmts stmts res) (if (empty? stmts) res (stm stmts res)))
(define (store-if-res stm-val res)
  (define-values (stmts val) (unwrap-result stm-val))
  (if res
      (maybe-stmts (append stmts (list (sham-store val res))) #f)
      (maybe-stmts stmts val)))
(define (syn-quote sym) #`'#,sym)
(define sham-closure-sym #`lazy-seq-closure)
(define sham-current-lazy-index #`lazy-seq-current-idx)
(define (cons-box! val blst) (set-box! blst (cons val (unbox blst))))
(define (snoc-box! val blst) (set-box! blst (append (unbox blst) (list val))))
;; append a list to the one inside a box
(define (append-box! blst lst) (set-box! blst (append (unbox blst) lst)))

(define (use-ptr-type? cry-type)
  (and
   (or (type? cry-type) (error 'sham/cryptol "checking ptr type for unknown ~a" cry-type))
   (or (type-tuple? cry-type)
       (and (type-sequence? cry-type)
            (not (small-int-type? cry-type))))))
(define (add-ptr-type sham-type (ptr-box? #t)) (if ptr-box? #`(ptr-type #,sham-type) sham-type))
(define (small-int-type? cry-type)
  (match cry-type
    [(type-sequence d (type-bit)) #t]
    ;; [(type-sequence d (type-bit)) (<= (maybe-dim-i d) 64)]
    [else #f]))

(define (to-sham-type cry-type (ptr-box? #f))
  (match cry-type
    [(type-bit) #`bit-type]
    [(type-tuple ts ...)
     (add-ptr-type #`(tuple-type #,@(map (curryr to-sham-type #t) ts)) ptr-box?)]
    [(type-sequence dim (type-bit))
     (define len (maybe-dim-i dim))
     (if (<= len 64)
         #`i64
         #`(ll-type-ref #,len)
         ;; (error 'todo "larger than 64 bit numbers ~a" len)
         )]
    [(type-sequence dim t)
     ;; #`basic-sequence-type
     ;; #`(ll-type-struct i64 (ll-type-pointer #,(to-sham-type t #t)))
     (add-ptr-type #`(seq-type #,(to-sham-type t #f)) ptr-box?)]
    [(type-integer) (printf "WARN: using generic integer type\n") (add-ptr-type #`i64 ptr-box?)]
    [else #`TODO-sham-type]))

(define (sham-function-arg-types up-type p-type ctxt)
  (match-define (list argts ... rst) (get-farg-types up-type))
  `(,@(map (λ (t) (to-sham-type t (use-ptr-type? t))) argts) ,(add-ptr-type (to-sham-type rst))))

(define (sham-store val ptr)
  (match ptr
    [(ref v) #`(store-val! #,(load-if-ref val) #,(load-if-ref v))]))
(define (sham-ref-sym v)
  (match v
    [(? symbol?) (syn-quote v)]
    [(? syntax?) v]
    [(? ast:id?) (syn-quote (ast-id-stxid v))]))
(define (sham-ref v) #`(expr-ref #,(sham-ref-sym v)))
(define sham-current-lazy-index-ref (sham-ref (syn-quote sham-current-lazy-index)))
(define (sham-store-integer! i type result ctxt)
  (match type
    [(? small-int-type?) (sham-store (sham-integer-literal i type) result)]
    [else (error 'todo-sham "storing big integer ~a ~a" i type)]))
(define (sham-integer-literal i type)
  (cond [(small-int-type? type) #`(ll-val-ui #,(number->string i) #,(to-sham-type type))]
        [(type-integer? type) (printf "WARN: using generic integer type: ~a\n" i) #`(ui64 #,i)]
        [else (error 'todo-sham "big integer ~a ~a" i type)]))
(define (sham-dim-value ctxt dim)
  ;; TODO for variable dims
  (define dim-i (or (maybe-dim-i dim)
                    (maybe-dim-i (try-specialize-type dim ctxt))))
  (unless dim-i (error 'sham/cry "couldn't specialize sequence dimension ~a" dim))
  #`(ui64 #,dim-i))

(define (allocat->let a) (match-define (allocat n v t) a) #`(#,n #,v #,t))
(define (allocats->let allocats . stmts) #`(stmt-let #,(map allocat->let allocats) #,@(flatten stmts)))

(define (int-ult-loop bodyf start end (step #`(ui64 1)))
  (define idx-sym #`'#,(gensym 'idx))
  (define idx #`(expr-ref #,idx-sym))
  #`(stmt-let
     ([#,idx-sym #,start i64])
     (stmt-while
      (op-icmp-ult #,idx #,end)
      (stmt-block #,@(bodyf idx)
                  (stmt-set! #,idx (op-add #,idx #,step))))))

;;  returns the let binds and stmts for internal allocation
;; -> (values val (listof allocat/c) (listof stmts))
;;  val: expr for the final allocated value, usually a ref to a name
;;  allocat/c: let bound values for all internal allocations
;;  stmts: assigining sub parts for internal allocations for complex types
(define (allocate-type ctxt type (in? #t))
  (define alloca-name #`'#,(gensym 'val))
  (define sham-type (to-sham-type type #f))
  (define alloca-val #`(alloca #,sham-type))
  (define alloca-type #`(ptr-type #,sham-type))
  (define basic-alloca (allocat alloca-name alloca-val alloca-type))
  (define alloca-ref (ref #`(expr-ref #,alloca-name)))

  ;; sub allocation for internal values
  (define istmts (if (and in? (use-ptr-type? type)) (allocate-in-type ctxt alloca-ref type) '()))

  (values alloca-ref (list basic-alloca) istmts))

;; allocate-in-type: allocate internal values for tuple/seq types
;; -> allocative stmts
(define (allocate-in-type ctxt val type)
  (match type
    [(type-tuple ts ...)
     (for/fold ([istmts '()])
               ([t ts]
                [i (length ts)]
                #:when (use-ptr-type? t))
       (define-values (ival allocats stmts) (allocate-type ctxt t))
       ;; peel one ref for both as we are storing the address of sub tuple
       (define set-stmt (sham-store (ref-v ival) (ref-v (sham-tuple-index type i val))))
       (append istmts (list (allocats->let allocats (append stmts (list set-stmt))))))]

    [(type-sequence dim t)
     #:when (not (type-bit? t))
     ;; sequence are boxed c array blocks with dimension value stored
     (define elem-type (to-sham-type t #f))
     (define dim-val (sham-dim-value ctxt dim))

     ;; sub array ptr
     (define ptr-name #`'#,(gensym 'sptr))
     (define ptr-val #`(op-array-alloca (expr-etype #,elem-type) #,dim-val))
     (define ptr-type #`(ptr-type #,elem-type))
     (define ptr-alloca (allocat ptr-name ptr-val ptr-type))
     (define ptr-ref #`(expr-ref #,ptr-name))

     (define ptr-stmt (sham-store #`(expr-ref #,ptr-name) (ref-v (sham-sequence-ptr val))))
     (define dim-stmt (sham-store dim-val (sham-sequence-dim val)))

     (define istmts
       (if (use-ptr-type? t)
           (let* ([idx-name #`'#,(gensym 'sqi)]
                  [idx-ref #`(expr-ref #,idx-name)]
                  [idx-val #`(ui64 0)]
                  [idx-type #`i64])
             (define set-sub
               #`(stmt-let
                  ([#,idx-name #,idx-val #,idx-type])
                  (stmt-while
                   (op-icmp-ult #,idx-ref #,dim-val)
                   (stmt-block
                    #,@(allocate-in-type (ref #`(op-gep #,ptr-ref #,idx-ref)) t)
                    (stmt-set! #,idx-ref (op-add #,idx-ref (ui64 1)))))))
             (list set-sub))
           '()))
     (list (allocats->let (list ptr-alloca) ptr-stmt dim-stmt istmts))]
    [it (list (sham-store #`(alloca #,(to-sham-type type #f)) val))]))

(define (empty-internal-context) (ic (box '()) (box '()) (box #f)))
(define (get-ic c) (cond [(ic? c) c]
                         [(cc? c) (cc-cctxt c)]
                         [else (error 'cryptol/sham "unknown ctxt ~a" c)]))

(define (get-closure ctxt) (unbox (ic-closure (get-ic ctxt))))
(define (set-closure! ctxt cl) (set-box! (ic-closure (get-ic ctxt)) cl))
(define (add-allocats! ctxt allocats) (append-box! (ic-allocats (get-ic ctxt)) allocats))
(define (add-allocatives! ctxt stmts) (append-box! (ic-allocatives (get-ic ctxt)) stmts))
(define (allocats ictxt) (unbox (ic-allocats (get-ic ictxt))))
(define (allocatives ictxt) (unbox (ic-allocatives (get-ic ictxt))))

;; generate allocations and store them in ctxt
(define (sham-allocate-type! ctxt type)
  (define-values (val allocats stmts) (allocate-type ctxt type))
  (add-allocats! ctxt allocats)
  (add-allocatives! ctxt stmts)
  val)

(define (allocate-lazy-seq! ctxt type seq-sym seq-fun)
  ;; (match-define (list name type syn-val) s)
  (match-define (type-sequence dim elem-type) type)
  (unless (and (type-sequence? type) (use-ptr-type? type))
    (error 'cryptol/lazy-sequence "sequence type is not complicated enough: ~a" (pretty-cry type)))
  (define clos-val (get-closure ctxt))
  (define l-val #`(expr-ref #,seq-sym))
  (define l-ref (ref l-val))
  (define b-type (to-sham-type elem-type))
  (define l-type #`(lazy-seq-type #,b-type))
  (define l-allocat (allocat seq-sym #`(alloca #,l-type) #`(ptr-type #,l-type)))
  (define b-stmts (allocate-in-type ctxt l-ref type)) ;; only top level values are lazy
  (define l-stmts (list (sham-store #`(ui64 0) (ref #`(lazy-seq-upto-ptr #,l-val)))
                        ;; (sham-store (ref #`(expr-ref #,seq-fun)) (ref #`(lazy-sequence-func-ptr #,l-val)))
                        ;; (sham-store (ref-v (closure-name clos-val)) (ref #`(lazy-sequence-clos-ptr #,l-val)))
                        ))
  (add-allocats! ctxt (cons l-allocat '()))
  (add-allocatives! ctxt (append b-stmts l-stmts))
  l-type)

(define (lookup-and-to-sham-type ctxt ev)
  (let ([t (lookup-typeof ctxt (env-var-name ev))])
    (printf "latst: ~a\n" t)
    (to-sham-type t (use-ptr-type? t))))
(define (create-closure! ctxt where-evs where-vals)
  (define cl-name (syn-quote sham-closure-sym))
  (define cl-val (ref #`(expr-ref #,cl-name)))
  (define binds (filter env-bind-var? (env-val (cc-env ctxt))))
  (define bind-types (map (curry lookup-and-to-sham-type ctxt) binds))
  (define where-types (for/list ([we where-evs]
                                 [wv where-vals])
                        (cond
                          [(laz-seq? wv) #`(ptr-type #,(laz-seq-t wv))]
                          [else (lookup-and-to-sham-type ctxt we)])))
  (define all-stored (append binds where-evs))
  (define ctypes (for/list ([v all-stored]) (lookup-typeof ctxt (env-var-name v))))
  ;; (define types (for/list ([t ctypes]) (to-sham-type t (use-ptr-type? t))))
  (define types (append bind-types where-types))
  ;; (printf "closure: ~a ~a\n" ctypes types) (error 'stop)
  (define cl-type #`(clos-type #,@types))
  (add-allocats! ctxt (list (allocat cl-name #`(alloca #,cl-type) #`(ll-type-pointer #,cl-type))))
  ;; (set-closure! ctxt cl-val)
  ;; (define (get-val ev)
  ;;   (match ev
  ;;     [(env-bind-var name val) val]
  ;;     [(env-where-var name val) (ref (sham-ref name))]))
  (define cl-stores
    (for/list ([ev all-stored]
               [ct ctypes]
               [st types]
               [i (length all-stored)])
      ;; (define v (get-val ev))
      (define v
        (unref-if-ptr
         (match ev
           [(env-bind-var name val) val]
           [(env-where-var name val) (ref (sham-ref name))])
         ct))
      (sham-store v ;; (if (use-ptr-type? ct) (ref-v v) v)
                  (ref #`(clos-index-val #,(ref-v cl-val) #,i)))))

  (add-allocatives! ctxt cl-stores)
  (set-closure! ctxt (closure cl-name ctxt types all-stored)))

(define (create-closure-binds clos body)
  (match-define (closure name ctxt types vars) clos)
  #`(stmt-let #, (cons #`(#,(syn-quote sham-closure-sym) lazy-func-clos (clos-type #,@types))
                       (for/list ([cv vars]
                                  [t types]
                                  [i (length vars)])
                         #`(#,(syn-quote (ast-id-stxid (env-var-name cv))) (lazy-func-clos-val #,i) #,t)))
              #,@body))

(define (create-lazy-seq-func ctxt seq-name seq-type laz-val body)
  (define clos (get-closure (get-ic ctxt)))
  (define-values (body-stmts body-res) (unwrap-result body))
  (when body-res (error 'sham/cryptol "got non #f body for lazy func"))
  (define idx-ref (sham-ref (syn-quote sham-current-lazy-index)))
  (define full-body
    (list
     #`(stmt-if (op-icmp-ult lazy-func-index lazy-func-upto)
                (stmt-void)
                (stmt-let ([#,(syn-quote sham-current-lazy-index) lazy-func-upto i64])
                          (stmt-while (op-icmp-ule #,idx-ref lazy-func-index)
                                      (stmt-block
                                       #,@body-stmts
                                       (stmt-set! #,idx-ref (op-add #,idx-ref (ui64 1)))
                                       (store-val! #,idx-ref (lazy-seq-upto-ptr lazy-func-seq))))))
     #`(stmt-return (op-load (sequence-index-ptr lazy-func-seq lazy-func-index)))))
  (define with-clos-body (create-closure-binds clos full-body))
  (match-define (laz-seq name ltype fname ct) laz-val)
  #`(make-def-function #,fname
                       (lazy-seq-func-type (clos-type #,@(closure-types clos)) #,(to-sham-type (type-sequence-t seq-type)))
                       #,with-clos-body))

(define (sham-sequence-dim val)
  (ref #`(sequence-len-ptr #,(load-if-ref (ref-v val))))
  ;; (ref #`(op-gep #,(load-if-ref (ref-v val)) (ui32 0) (ui32 0)))
  )
(define (sham-sequence-ptr val)
  (ref (ref #`(sequence-array-ptr-ptr #,(load-if-ref (ref-v val)))))
  ;; (ref (ref #`(op-gep #,(load-if-ref (ref-v val)) (ui32 0) (ui32 1))))
  )

(define (imed-i32 i) (if (integer? i) #`(ui32 #,i) (load-if-ref i)))

(define (sham-index-value i) (if (integer? i) #`(ui32 #,i) (load-if-ref i)))
(define (sham-sequence-index-ref type i val)
  (printf "ssi: ~a ~a ~a\n" type i val)
  (unless (type-sequence? type) (error 'sham/cryptol "incorrect type for sequence ~a ~a ~a" type i val))
  (define addr #`(sequence-index-ptr #,(load-if-ref (ref-v val)) #,(sham-index-value i)))
  ;; (define addr #`(op-gep #,(load-if-ref (ref-v (sham-sequence-ptr val)))
  ;;                        #,(if (integer? i) #`(ui32 #,i) #`(op-int-cast #,i (expr-etype i32)))))
  (if (use-ptr-type? (maybe-sequence-elem-type type)) (ref (ref addr)) (ref addr))
  (ref addr))

(define (sham-sequence-index-laz type i val)
  (match val
    [(laz-seq name ltype func-name ctxt)
     (define clos (get-closure ctxt))
     (define val #`(expr-op #,func-name #,name #,(sham-index-value i) #,(sham-ref (closure-name clos))))
     (ref-if-ptr val (maybe-sequence-elem-type type))]
    [(laz-slc laz offst)
     (sham-sequence-index-laz type #`(op-sub #,(sham-index-value i) #,(sham-dim-value #f offst)) laz)]))
(define (sham-sequence-index type i val)
  (cond [(ref? val) (sham-sequence-index-ref type i val)]
        [(laz? val) (sham-sequence-index-laz type i val)]
        [else (error 'sham/cryptol "unknown type of sequence value: ~a" val)]))

(define (sham-sequence-index-lhs type idx seq)
  ;; (printf "sil: ~a ~a ~a\n" type idx seq)
  (match seq
    [(laz-slc laz offst) (sham-sequence-index-lhs (maybe-sequence-elem-type type)
                                                  #`(op-sub #,(sham-index-value idx) #,(sham-dim-value #f offst))
                                                  laz)]
    [(laz-seq name ltype func-name ctxt)
     (define addr #`(sequence-index-ptr #,(sham-ref name) #,(sham-index-value idx)))
     (if (use-ptr-type? (maybe-sequence-elem-type type)) (ref (ref addr)) (ref addr))]
    [(? ref?) (sham-sequence-index-ref type idx seq)]))

(define (sham-sequence-index-set type seq idx val)
  (cond [(laz? seq) (sham-store val (sham-sequence-index-lhs type idx seq))]))
(define (laz-seq-balance-offset i seq)
  (match seq
    [(laz-slc l ofst) (laz-seq-balance-offset #`(op-sub #,i #,(sham-dim-value #f ofst)) l)]
    [else i]))
(define (peel-laz-slc seq)
  (match seq
    [(laz-slc l _) (peel-laz-slc l)]
    [else seq]))
;; just returns the pointer no load
(define (sham-tuple-index type index val)
  (define addr
    #`(tuple-index-ptr #,(load-if-ref (ref-v val)) #,(imed-i32 index))
    ;; #`(op-gep
    ;;    ;; peel one ref as gep itself needs a pointer
    ;;    #,(load-if-ref (match val [(ref val) val]))
    ;;    (ui32 0)
    ;;    (ui32 #,index))
    )
  (if (use-ptr-type? (maybe-tuple-type-i type index)) (ref (ref addr)) (ref addr)))

(define (compare-for-type type val1 val2)
  (define (andn cs)
    (match cs
      [(list c1) c1]
      [(cons fst rst) #`(op-and #,fst #,(andn rst))]
      ['() #`primitive-true]))
  (match type
    [(type-bit) #`(op-icmp-eq #,(load-if-ref val1) #,(load-if-ref val2))]
    [(type-tuple ts ...)
     (andn
       (for/list ([t ts] [i (length ts)])
         (define (geti val) (sham-tuple-index type i val))
         (compare-for-type t (geti val1) (geti val2))))]
    [(type-sequence (dim-int i) (type-bit))
     #:when (small-int-type? type)
     #`(op-icmp-eq #,(load-if-ref val1) #,(load-if-ref val2))]
    [(type-sequence (dim-int d) t)
     (andn (for/list ([i d])
             (define (geti val) (sham-sequence-index type i val))
             (compare-for-type t (geti val1) (geti val2))))]
    [else (error 'sham/cryptol "TODO compare for: ~a" (pretty-cry type))]))

(define (do-result-stmts rs)
  (for/fold ([stmts '()]
             [vals '()]
             #:result (values (reverse stmts) (reverse vals)))
            ([r rs])
    (define-values (ss r^) (unwrap-result r))
    ;; if result is false then stmt is expr
    (values (append ss stmts) (cons r^ vals))))

(define-compiler sham-compiler
  [(internal-def-context name type ctxt) (empty-internal-context)]
  [(def-val ctxt orig-name new-name up-type orig-type pargs-evs cval internal-ctxt)
   (define-values (val-stmts val-res) (unwrap-result cval))
   (with-syntax
     ([name-stxid (ast-id-stxid new-name)]
      [(arg-types ...) (sham-function-arg-types up-type orig-type ctxt)]
      [(tvar-args ...) (map ast-id-stxid (map env-var-name pargs-evs))] ;; TODO only need type vars with kind int
      [(tvar-vals ...) (map env-var-val pargs-evs)]
      [(tvar-types ...) (map (const #`ll-i64) pargs-evs)]
      [(bodys ...) val-stmts])
     #`(make-def-function
        'name-stxid (basic-function-type arg-types ...)
        (basic-function-body #,(map allocat->let (allocats internal-ctxt))
                             #,@(allocatives internal-ctxt) bodys ...)))]
  [(def-test-results ctxt type)
   ;; do them twice for gensym
   (cons (sham-allocate-type! ctxt type) (sham-allocate-type! ctxt type))]
  [(def-test ctxt name type cr1 r1 cr2 r2)
   (define-values (stmt1 val1) (unwrap-result cr1))
   (define-values (rstmt1 rval1) (unwrap-result r1))
   (define-values (stmt2 val2) (unwrap-result cr2))
   (define-values (rstmt2 rval2) (unwrap-result r2))

   (with-syntax ([name-str (format "~a" (syntax-e (ast-id-stxid name)))])
     (allocats->let (allocats ctxt)
                    (allocatives ctxt)
                    stmt1 rstmt1 stmt2 rstmt2
                    #`(stmt-if #,(compare-for-type type (or val1 rval1) (or val2 rval2))
                               (print-pass name-str)
                               (print-fail name-str))))]

  [(tests ctxt ts)
   #`(make-def-function 'run-tests test-function-type (stmt-block #,@ts (stmt-return-void)))]

  [(function-arg ctxt name index arg-type function-type)
   (define arg-val #`(expr-ref #,index))
   (add-allocats! ctxt (list (allocat (sham-ref-sym name) arg-val (to-sham-type arg-type #t))))
   (define val-stx (sham-ref name))
   (printf "function-arg: ~a ~a\n" val-stx (use-ptr-type? arg-type))
   (if (use-ptr-type? arg-type) (ref val-stx) val-stx)]
  [(function-result ctxt res-type func-type) (ret #`(expr-ref #,(sub1 (length (get-farg-types func-type)))))]

  [(where-val-env ctxt tv-defs)
   (printf "where-val-env: ~a\n" tv-defs)
   (define tv-typeofs (for/list ([tv tv-defs]) (env-where-var (def-typed-val-name tv) (def-typed-val-type tv))))
   (define (is-seq? vl) (and (type-sequence? (def-typed-val-type vl))
                             (use-ptr-type? (def-typed-val-type vl))))
   (define seqs (filter is-seq? tv-defs))
   (define non-seqs (filter-not is-seq? tv-defs))

   (define (lazy-seq tdef)
     (match-define (def-typed-val name type value) tdef)
     (define (name-quote vl (f identity)) (syn-quote (f (ast-id-stxid vl))))
     (define seq-name (name-quote name))
     (define func-name (name-quote name (compose gensym syntax->datum (λ (s) (format-id #f "laz-~a" s)))))
     ;; function to force values
     (define ltype (allocate-lazy-seq! ctxt type seq-name func-name))
     ;; (map (λ (name value fn) ( name value (sham-ref fn) clos-val)) tv-defs seq-vals seq-funs)
     ;; (cons seq-ref func-ref)
     (laz-seq (sham-ref seq-name) ltype func-name ctxt))
   (define (try-lazy-seq tdef)
     (if (is-seq? tdef) (lazy-seq tdef) (error 'sham/cryptol "unknown type of value in cry where")))

   (define lvals (map try-lazy-seq tv-defs))
   (unless (empty? seqs) (create-closure! ctxt tv-typeofs lvals))
   (values (update-env ctxt
                        #:val (for/list ([lv lvals]
                                         [tv tv-defs])
                                (env-where-var (def-typed-val-name tv) lv)))
           lvals)
   ;; (values
   ;;  (update-env
   ;;   (if (empty? seqs)
   ;;       (update-env ctxt #:val (lazy-seqs seqs))
   ;;       ctxt)
   ;;   #:val (normal-vars non-seqs))
   ;;  var-triples)
   ]
  [(where-val-res ctxt name type pval)
   (or pval (sham-ref name))]

  [(where-val ctxt name type prep-val c-val)
   (if (and (type-sequence? type) (use-ptr-type? type))
       (let ([sf (create-lazy-seq-func ctxt name type prep-val c-val)])
         (add-lifts! ctxt (env-var name sf))
         prep-val)
       (error 'sham/cryptol "unknown where val"))]
  [(expr-result ctxt arg-type has-result)
   (printf "expr-result-type: ~a ~a ~a\n" (pretty-cry arg-type) has-result (use-ptr-type? arg-type))
   ;; #f if result is an immediate and does not need a store op
   (or has-result (and (use-ptr-type? arg-type) (sham-allocate-type! ctxt arg-type)))]
  [(expr-app ctxt rator rands)
   (printf "sham-expr-app: ~a ~a\n" rator rands)
   (define-values (rand-stmts rand-vals^) (do-result-stmts (map cdr rands)))
   (define-values (stmts result-val) (unwrap-result (cc-res ctxt)))
   (match rator
     [(env-special-var name val type oname otype pargs)
      (define rand-vals
        (map (λ (v1 v2 t) (load-if-needed (or v1 v2) t))
             rand-vals^
             (map car rands)
             (drop-right (get-farg-types type) 1)))
      (define e
        (match result-val
          [(ref ptr) #`(expr-op '#,(ast-id-stxid name) #,@rand-vals #,ptr)]
          [(? syntax? val) (error 'TODO "got stx for app result: ~a ~a" rator rands)]
          [#f (error 'TODO "no result, need allocation: ~a ~a" rator rands)]))
      (stm (append stmts rand-stmts (list #`(stmt-expr #,e))) #f)]
     [(env-var name val)
      ;; #`(expr-op #,(ast-id-stxid name) #,@rand-vals)
      (error 'todo "app env-var ~a" rator)])]
  [(expr-app-primitive ctxt prim-val poly-vars up-type arg-exprs fcompile)
   (printf "app-prim: ~a ~a ~a ~a\n" prim-val up-type poly-vars (map pretty-cry arg-exprs))
   ((hash-ref primitive-apps (syntax->datum (ast-id-stxid (env-var-name prim-val))))
    ctxt prim-val poly-vars up-type arg-exprs fcompile)]
  [(expr-bind ctxt value) value]
  [(expr-var ctxt name env-value)
   (define-values (stmts result) (unwrap-result (cc-res ctxt)))
   (define type (cc-type ctxt))
   (define name-stx (ast-id-stxid name))
   (printf "expr-var: ~a ~a ~a ~a\n" name (pretty-cry type) env-value result)
   (define immediate-value
     (match env-value
       [(env-primitive-var name type) (format-id #f "primitive-~a" name-stx)]
       ;; [(env-lazy-var name val ast) (laz env-value) ;; (error 'sham/cryptol "lazy var for var")
       ;;                              ]
       [(env-bind-var name val) val]
       [(env-var name val) (load-if-ref val)]
       [else (error 'sham/cryptol "TODO-expr-var: ~a" env-value)]))
   (match result
     [(ref ptr) (stm (append stmts (list (sham-store immediate-value result))) #f)]
     [#f (maybe-stmts stmts immediate-value)])]

  [(tuple-literal ctxt args)
   (define t (cc-type ctxt))
   (define-values (stmts res) (unwrap-result (cc-res ctxt)))
   (define-values (arg-stmts arg-vals) (do-result-stmts args))
   (define (stores res-name)
     (for/list ([val arg-vals]
                [i (length arg-vals)])
       (match val
         [#f #f]
         [(? syntax?)
          (sham-store val (sham-tuple-index t i res-name))
          ;; #`(stmt-expr (op-store! #,val #,(ref-v (sham-tuple-index t i res-name))))
          ])))
   (if res
       (stm (append stmts arg-stmts (filter-not false? (stores res))) #f)
       (error 'sham/sam "tuple-literal needs allocated result to store ~a" args))]
  [(tuple-index ctxt value type index)
   (define-values (stmts r) (unwrap-result value))
   ;; (printf "tuple-index: ~a ~a,~a ~a\n" r type (cc-type ctxt) index)
   (maybe-stmts stmts (sham-tuple-index type index r))]


  [(integer-literal ctxt i)
   (define-values (stmts res) (unwrap-result (cc-res ctxt)))
   (define typ (cc-type ctxt))
   (if res
       (maybe-stmts (append stmts (list (sham-store-integer! i typ res ctxt))) #f)
       (sham-integer-literal i typ))]

  [(sequence-index ctxt value type index)
   (define-values (stmts r) (unwrap-result value))
   (printf "seq-i: ~a ~a ~a\n" value type index)
   (maybe-stmts stmts (sham-sequence-index type index r))]
  [(sequence-index-result ctxt value type index)
   (define-values (stmts r) (unwrap-result value))
   (printf "seq-ir: ~a ~a ~a\n" value type index)
   (maybe-stmts stmts (sham-sequence-index-lhs type index r))]
  [(sequence-basic ctxt args)
   (define type (cc-type ctxt))
   (define-values (stmts res^) (unwrap-result (cc-res ctxt)))
   (define res (or res^ (sham-allocate-type! ctxt type)))
   (define-values (arg-stmts arg-vals) (do-result-stmts args))
   (printf "seq-b: ~a ~a ~a\n" args type res)
   (define (stores res-name)
     (for/list ([val arg-vals]
                [i (length arg-vals)])
       ;; (printf "seq-store: ~a ~a\n" res-name val)
       (match val
         [#f #f]
         [(? syntax?) (sham-store val (sham-sequence-index type i res-name))])))
   (if res
       (maybe-stmts (append stmts arg-stmts (filter-not false? (stores res))) (if res^ #f res))
       (error 'sham/cryptol "basic-sequence needs allocated result ~a" args))]
  [(sequence-enum ctxt from step to)
   (define (iv v) (expr-lit-v v))
   (define type (cc-type ctxt))
   (define elem-type (maybe-sequence-elem-type type))
   (define-values (stmts res^) (unwrap-result (cc-res ctxt)))
   (define res (or res^ (sham-allocate-type! ctxt type)))
   (define fi (iv from))
   (define si (iv step))
   (define ti (iv to))
   (define len (/ (add1 (- ti fi)) si))
   (printf "seq-en: ~a ~a\n" type (list fi si ti))
   (define loop-stmt
     (int-ult-loop
      (λ (idx) (list
                (sham-store #`(op-mul #,(sham-integer-literal si elem-type)
                                      (op-add #,(sham-integer-literal fi elem-type) #,idx))
                            (sham-sequence-index type idx res))))
      #`(ui64 0) #`(ui64 #,len)) )
   (maybe-stmts (append stmts (list loop-stmt)) (if res^ #f res))]

  [(sequence-comp-index ctxt type)
   (define-values (res-stmts res-val) (unwrap-result (cc-res ctxt)))
   (if (laz? res-val)
       (laz-seq-balance-offset (sham-ref (syn-quote sham-current-lazy-index)) res-val)
       (let ([idx-sym #`'#,(gensym 'sci)])
         #`(expr-ref #,idx-sym)))]
  [(sequence-comp-var ctxt vv idx)
   (match-define (list var res val typ) vv)
   (define-values (val-stmts val-res) (unwrap-result val))
   (printf "seq-c-var: ~a ~a\n" vv idx)
   (sham-sequence-index typ idx (or val-res res))]
  [(sequence-comp-result ctxt idx type)
   (define-values (stmts res) (unwrap-result (cc-res ctxt)))
   (printf "seq-c-res: ~a ~a ~a\n" res idx type)
   (maybe-stmts
    stmts
    (if (laz? res)
        (laz-ref (peel-laz-slc res) sham-current-lazy-index-ref)
        (sham-sequence-index type idx res)))]

  [(sequence-comp ctxt body vvs idx body-res)
   (define type (cc-type ctxt))
   (define len (sham-dim-value ctxt (type-sequence-dim type)))
   (define-values (body-stmts body-val) (unwrap-result body))
   (define-values (res-stmts res-val) (unwrap-result body-res))
   (define vvs-stmts
     (apply append
            (for/list ([vv vvs])
              (match-define (list var res val typ) vv)
              (define-values (val-stmts val-res) (unwrap-result val))
              val-stmts)))
   (define (loop-stmt)
     (define idx-sym (syntax-case idx () [(expr-ref is) #`is]))
     #`(stmt-let ([#,idx-sym (ui64 0) i64])
                 (stmt-while (op-icmp-ult #,idx #,len)
                             (stmt-block
                              #,@body-stmts
                              #,@(if body-val (list (sham-store body-val res-val)) '())
                              (stmt-set! #,idx (op-add #,idx (ui64 1)))))))
   (define (lazy-stmt)
     ;; (when body-val
     ;;   (error 'cry/todo "sequence comp returned a value instead of setting ~a ~a" res-val body-val))
     (match-define (laz-ref lseq lidx) res-val)
     (define res-stmt
       (if body-val
           (list (sham-sequence-index-set type lseq lidx body-val))
           '()))
     #`(stmt-block #,@body-stmts #,@res-stmt))
   (maybe-stmts (append res-stmts vvs-stmts (list (if (laz? res-val) (lazy-stmt) (loop-stmt)))) #f)
   ;; (error 'sham/cryptol "TODO sequence-comp ~a ~a" body vvs)
   ]
  [(sequence-str ctxt str) (error 'sham/cryptol "sequence-str ~a" str)])

(define require-syntax
  #`((require sham
              sham/jit
              sham/cryptol/prelude/sham
              (prefix-in ll- sham/llvm/ir))))

(define extra-syntax
  #`((module+ test
       (define built-sham-env (build-sham-env cryptol-module #:debug #t))
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-llvm-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       ;; (sham-env-optimize-llvm! built-sham-env #:opt-level 2)
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-opt-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       (define built-jit-env (initialize-jit built-sham-env))
       (define test-runner (jit-lookup-function built-jit-env 'run-tests))
       (printf "running-cryptol-tests:\n")
       (test-runner))))

(define primitive-defs
  #`((ll-def-external 'printf (ll-type-function (ll-type-pointer i8) #t i64))))

(define (poly-var-value var pvars) (maybe-first-env-vars (lookup-env-vars pvars var)))

(define (for-lazy-seq normf lazyf)
  (λ args
    (define-values (res-stmts res) (unwrap-result (cc-res (first args))))
    (printf "for-lazy-seq:~a ~a\n" (object-name lazyf) res)
    (apply (if (laz? res) lazyf normf) args)))
(define (append-sequence-normal ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (match-define (type-poly (front back elem) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (front-dim back-dim elem-type) (values (get-val front) (get-val back) (get-val elem)))
  (define-values (front-type back-type) (values (type-sequence front-dim elem-type) (type-sequence back-dim elem-type)))
  (match-define (list fst snd) arg-exprs)
  (define-values (fst-ref fst-allocs fst-alc-stmts) (allocate-type ctxt front-type #f))
  (define-values (snd-ref snd-allocs snd-alc-stmts) (allocate-type ctxt back-type #f))
  (define-values (fst-stmts fst-val) (unwrap-result (fcompile fst front-type fst-ref)))
  (define-values (snd-stmts snd-val) (unwrap-result (fcompile snd back-type snd-ref)))
  (when (or fst-val snd-val) (error 'sham/cryptol "append should just store in result ~a ~a" fst-val snd-val))
  ;; aliasing pointers for append where the pointers for front and back are just sub parts of original result
  (maybe-stmts
   (append res-stmts
           (list
            (allocats->let (append fst-allocs snd-allocs)
                           ;; store dim and res pointer
                           fst-alc-stmts
                           (sham-store (sham-dim-value ctxt front-dim) (sham-sequence-dim fst-ref))
                           (sham-store (load-if-ref (ref-v (sham-sequence-ptr res))) (ref-v (sham-sequence-ptr fst-ref)))

                           ;; alias the current result pointer at dim
                           snd-alc-stmts
                           (sham-store (sham-dim-value ctxt back-dim) (sham-sequence-dim snd-ref))
                           (sham-store #`(op-gep #,(load-if-ref (ref-v (sham-sequence-ptr res)))
                                                 #,(sham-dim-value ctxt front-dim))
                                       (ref-v (sham-sequence-ptr snd-ref)))

                           fst-stmts
                           snd-stmts)))
   #f))
(define (append-sequence-lazy ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  ;; (error 'todo "lazy seq append")
  ;; calculate upto #`lazy-func-index
  (match-define (type-poly (front back elem) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (front-dim back-dim elem-type) (values (get-val front) (get-val back) (get-val elem)))
  (define-values (front-type back-type) (values (type-sequence front-dim elem-type) (type-sequence back-dim elem-type)))
  (match-define (list fst snd) arg-exprs)
  (define (body-at idx)
    ;; (define laz-val (laz-ref res idx))
    (define fst-res res)
    (define snd-res (laz-slc res front-dim))
    (define-values (fst-stmts fst-val) (unwrap-result (fcompile fst front-type fst-res)))
    (define-values (snd-stmts snd-val) (unwrap-result (fcompile snd back-type snd-res)))
    ;; (error 'stop)
    #`(stmt-if (op-icmp-ult #,idx #,(sham-dim-value ctxt front-dim))
               (stmt-block #,@fst-stmts)
               (stmt-block #,@snd-stmts)))
  ;; (define loop-stmt
  ;;   (int-ult-loop loopf #`lazy-func-seq-upto #`lazy-func-index))
  (maybe-stmts (append res-stmts (list (body-at (sham-ref (syn-quote sham-current-lazy-index))))) #f))

(define append-sequence (for-lazy-seq append-sequence-normal append-sequence-lazy))

(define (join-sequence ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (match-define (type-poly (parts each elem) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (parts-dim each-dim elem-type) (values (get-val parts) (get-val each) (get-val elem)))
  (define sub-type (type-sequence parts-dim (type-sequence each-dim elem-type)))
  (define-values (sub-ref sub-allocats sub-alo-stmts) (allocate-type ctxt sub-type))
  (define loop-stmt
    (int-ult-loop
     (λ (idx)
       (list
        (sham-store #`(op-gep #,(load-if-ref (ref-v (sham-sequence-ptr res)))
                              (op-mul #,idx #,(sham-dim-value ctxt each-dim)))
                    (ref-v (sham-sequence-ptr (sham-sequence-index sub-type idx sub-ref))))))
     #`(ui64 0)
     (sham-dim-value ctxt parts-dim)))
  (define-values (sub-stmts sub-val) (unwrap-result (fcompile (car arg-exprs) sub-type sub-ref)))
  (when sub-val (error 'sham/cryptol "join: sub should just store in result: ~a ~a" arg-exprs poly-vars))
  (maybe-stmts (list (allocats->let sub-allocats sub-alo-stmts loop-stmt sub-stmts)) #f))

(define (split-sequence ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (match-define (type-poly (parts each elem) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (parts-dim each-dim elem-type) (values (get-val parts) (get-val each) (get-val elem)))
  (define sub-type (type-sequence (dim-int (* (maybe-dim-i parts-dim) (maybe-dim-i each-dim))) elem-type))
  (define-values (sub-ref sub-allocats sub-alo-stmts) (allocate-type ctxt sub-type))
  (define-values (sub-stmts sub-val) (unwrap-result (fcompile (car arg-exprs) sub-type sub-ref)))
  (define loop-stmt
    (int-ult-loop
     (λ (idx-each)
       (list
        (int-ult-loop
         (λ (idx-part)
           (list
            (sham-store
             (load-if-ref
              (sham-sequence-index
               sub-type
               #`(op-add (op-mul #,idx-part #,(sham-dim-value ctxt each-dim)) #,idx-each) sub-ref))
             (sham-sequence-index (type-sequence each-dim elem-type)
                                  idx-each
                                  (sham-sequence-index
                                   (type-sequence parts-dim (type-sequence each-dim elem-type))
                                   idx-part
                                   res)))))
         #`(ui64 0)
         (sham-dim-value ctxt parts-dim))))
     #`(ui64 0)
     (sham-dim-value ctxt each-dim)))
  (when sub-val (error 'sham/cryptol "split: sub should just store"))
  (maybe-stmts (list (allocats->let sub-allocats sub-alo-stmts sub-stmts loop-stmt)) #f))

(define (index-sequence ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (match-define (type-poly (n a ix) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (n-dim a-type) (values (get-val n) (get-val a)))
  (define-values (sub-stmts sub-val) (unwrap-result (fcompile (first arg-exprs) (type-sequence n-dim a-type) #f)))
  (define-values (idx-stmts idx-val) (unwrap-result (fcompile (second arg-exprs) (type-sequence (dim-int 64) (type-bit)) #f)))
  (maybe-stmts (apply append res-stmts sub-stmts idx-stmts) (sham-sequence-index (type-sequence n-dim a-type) idx-val sub-val)))

(define (reverse-index-sequence ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (match-define (type-poly (n a ix) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (n-dim a-type) (values (get-val n) (get-val a)))
  (define-values (sub-stmts sub-val) (unwrap-result (fcompile (first arg-exprs) (type-sequence n-dim a-type) #f)))
  (define-values (idx-stmts idx-val) (unwrap-result (fcompile (second arg-exprs) (type-sequence (dim-int 64) (type-bit)) #f)))
  (printf "ris: ~a ~a\n" res (apply append res-stmts sub-stmts idx-stmts))
  (store-if-res
   (maybe-stmts (apply append res-stmts sub-stmts idx-stmts)
                (sham-sequence-index (type-sequence n-dim a-type)
                                     #`(op-sub (op-sub #,(sham-dim-value ctxt n-dim) (ui64 1)) #,idx-val) sub-val))
   res))

(define (drop-sequence ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (match-define (type-poly (front back elem) rst) (env-var-val prim-val))
  (define (get-val v) (poly-var-value v poly-vars))
  (define-values (front-dim back-dim elem-type) (values (get-val front) (get-val back) (get-val elem)))
  (define sub-type (type-sequence (dim-int (+ (maybe-dim-i front-dim) (maybe-dim-i back-dim))) elem-type))

  (define-values (sub-ref sub-allocats sub-alo-stmts) (allocate-type ctxt sub-type))
  (define-values (sub-stmts sub-val) (unwrap-result (fcompile (car arg-exprs) sub-type sub-ref)))

  (define memcpy-stmt #`(expr-app "memcpy" (ll-type-function ll-void* ll-void* i64 #f ll-void*)
                                  (sham-sequence-ptr res)
                                  (op-gep (sham-sequence-ptr sub-ref) front-dim)))
  (error 'sham/cryptol "todo")
  (maybe-stmts '() #f))

(define ((binary-int-primitive op) ctxt prim-val poly-vars up-type arg-exprs fcompile)
  (define-values (res-stmts res) (unwrap-result (cc-res ctxt)))
  (define type (cc-type ctxt))
  (define-values (fst-stmts fst-val) (unwrap-result (fcompile (first arg-exprs) type #f)))
  (define-values (snd-stmts snd-val) (unwrap-result (fcompile (first arg-exprs) type #f)))
  (maybe-stmts (append res-stmts fst-stmts snd-stmts) #`(#,op #,fst-val #,snd-val)))

(define primitive-apps
  (hash '<> append-sequence
        'join join-sequence
        'split split-sequence
        '\@ index-sequence
        '! reverse-index-sequence

        '+ (binary-int-primitive #`op-add)
        '- (binary-int-primitive #`op-sub)
        '* (binary-int-primitive #`op-mul)
        ))
