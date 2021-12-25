#lang racket

(require sham/sam/runtime/identifier
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
(struct ret ref [])                     ;; special for function return value, it is always a reference
(struct stm res [stmts res])

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
(define (maybe-stmts stmts res) (if (empty? stmts) res (stm stmts res)))

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
    [else #`TODO-sham-type]))

(define (sham-function-arg-types up-type p-type ctxt)
  (match-define (list argts ... rst) (get-farg-types up-type))
  `(,@(map (λ (t) (to-sham-type t (use-ptr-type? t))) argts) ,(add-ptr-type (to-sham-type rst))))

(define (sham-dim-i dim ctxt)
  (match dim
    [(dim-int i) #`(ui32 #,i)]
    [else (error 'sham/cry "todo dim based on poly variables ~a" (pretty-cry dim))]))

(define (sham-store val ptr)
  (match ptr
    [(ref v) #`(store-val! #,val #,(load-if-ref v))]))

(define (sham-store-integer! i type result ctxt)
  (match type
    [(? small-int-type?) (sham-store (sham-integer-literal i type) result)]
    [else (error 'todo-sham "storing big integer ~a ~a" i type)]))
(define (sham-integer-literal i type)
  (cond [(small-int-type? type) #`(ll-val-ui #,(number->string i) #,(to-sham-type type))]
        [else (error 'todo-sham "big integer ~a ~a" i type)]))
(define (sham-dim-value ctxt dim)
  ;; TODO for variable dims
  #`(ui64 #,(maybe-dim-i dim)))

;; alloca triplet for let binds
(struct allocat [name val type] #:transparent)

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
(define (allocate-type ctxt type)
  (define alloca-name #`'#,(gensym 'val))
  (define sham-type (to-sham-type type #f))
  (define alloca-val #`(alloca #,sham-type))
  (define alloca-type #`(ptr-type #,sham-type))
  (define basic-alloca (allocat alloca-name alloca-val alloca-type))
  (define alloca-ref (ref #`(expr-ref #,alloca-name)))

  ;; -> allocative stmts
  (define (allocate-in val itype)
    (match itype
      [(type-tuple ts ...)
       (for/fold ([istmts '()])
                 ([t ts]
                  [i (length ts)]
                  #:when (use-ptr-type? t))
         (define-values (ival allocats stmts) (allocate-type ctxt t))
         ;; peel one ref for both as we are storing the address of sub tuple
         (define set-stmt (sham-store (ref-v ival) (ref-v (sham-tuple-index itype i val))))
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
                      #,@(allocate-in (ref #`(op-gep #,ptr-ref #,idx-ref)) t)
                      (stmt-set! #,idx-ref (op-add #,idx-ref (ui64 1)))))))

               (list set-sub))
             '()))
       (list (allocats->let (list ptr-alloca) ptr-stmt dim-stmt istmts))]
      [it (list (sham-store #`(alloca #,(to-sham-type itype #f)) val))]))

  ;; sub allocation for internal values
  (define istmts (if (use-ptr-type? type) (allocate-in alloca-ref type) '()))

  (values alloca-ref (list basic-alloca) istmts))

;; internal context
(struct ic [allocats allocatives] #:transparent)
(define (empty-internal-context) (ic (box '()) (box '())))
(define (get-ic c) (cond [(ic? c) c]
                         [(cc? c) (cc-cctxt c)]
                         [else (error 'cryptol/sham "unknown ctxt ~a" c)]))

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

(define (sham-sequence-dim val)
  (ref #`(op-gep #,(load-if-ref (ref-v val)) (ui32 0) (ui32 0))))
(define (sham-sequence-ptr val)
  (ref (ref #`(op-gep #,(load-if-ref (ref-v val)) (ui32 0) (ui32 1)))))
(define (sham-sequence-index type i val)
  (define addr #`(op-gep #,(load-if-ref (ref-v (sham-sequence-ptr val)))
                         #,(if (integer? i) #`(ui32 #,i) #`(op-int-cast #,i (expr-etype i32)))))
  (if (use-ptr-type? (maybe-sequence-elem-type type)) (ref (ref addr)) (ref addr))
  (ref addr))
;; just returns the pointer no load
(define (sham-tuple-index type index val)
  (define addr
    #`(op-gep
       ;; peel one ref as gep itself needs a pointer
       #,(load-if-ref (match val [(ref val) val]))
       (ui32 0)
       (ui32 #,index)))
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
             (define (geti val) (sham-sequence-index t i val))
             (compare-for-type t (geti val1) (geti val2))))]
    [else (error 'sham/cryptol "TODO compare for: ~a" (pretty-cry type))]))

(define (do-result-stmts rs)
  (for/fold ([stmts '()]
             [vals '()]
             #:result (values stmts (reverse vals)))
            ([r rs])
    (define-values (ss r^) (unwrap-result r))
    ;; if result is false then stmt is expr
    (values (append ss stmts) (cons r^ vals))))

(define-compiler sham-compiler
  [(internal-def-context name type ctxt) (empty-internal-context)]
  [(def-val ctxt orig-name new-name up-type orig-type pargs-evs cval internal-ctxt)
   (with-syntax
     ([name-stxid (ast-id-stxid new-name)]
      [(arg-types ...) (sham-function-arg-types up-type orig-type ctxt)]
      [(tvar-args ...) (map ast-id-stxid (map env-var-name pargs-evs))] ;; TODO only need type vars with kind int
      [(tvar-vals ...) (map env-var-val pargs-evs)]
      [(tvar-types ...) (map (const #`ll-i64) pargs-evs)]
      [(bodys ...) (stm-stmts cval)])
     #`(make-def-function
        'name-stxid (basic-function-type arg-types ...)
        (basic-function-body #,(map allocat->let (allocats internal-ctxt)) #,@(allocatives internal-ctxt) bodys ...)))]
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

  [(function-arg ctxt arg-type index function-type)
   (define stx #`(expr-ref #,index))
   (if (use-ptr-type? arg-type) (ref stx) stx)]
  [(function-result ctxt res-type func-type) (ret #`(expr-ref #,(sub1 (length (get-farg-types func-type)))))]

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
  [(expr-bind ctxt value) value]
  [(expr-var ctxt name env-value)
   (define-values (stmts result) (unwrap-result (cc-res ctxt)))
   (define name-stx (ast-id-stxid name))
   (printf "expr-var: ~a ~a ~a\n" name env-value result)
   (define immediate-value
     (load-if-ref
      (match env-value
        [(env-primitive-var name type) (format-id #f "primitive-~a" name-stx)]
        [(env-var name val) val]
        [else (error 'sham/cryptol "TODO-expr-var: ~a" env-value)])))
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
   (maybe-stmts stmts (sham-sequence-index type index r))]
  [(sequence-basic ctxt args)
   (define t (cc-type ctxt))
   (define-values (stmts res) (unwrap-result (cc-res ctxt)))
   (define-values (arg-stmts arg-vals) (do-result-stmts args))
   (define (stores res-name)
     (for/list ([val arg-vals]
                [i (length arg-vals)])
       (printf "seq-store: ~a ~a\n" res-name val)
       (match val
         [#f #f]
         [(? syntax?) (sham-store val (sham-sequence-index t i res-name))])))
   (if res
       (maybe-stmts (append stmts arg-stmts (filter-not false? (stores res))) #f)
       (error 'sham/cryptol "basic-sequence needs allocated result ~a" args))]
  [(sequence-enum ctxt from step to)
   (define (iv v) (expr-lit-v v))
   (define type (cc-type ctxt))
   (define elem-type (maybe-sequence-elem-type type))
   (define-values (stmts res) (unwrap-result (cc-res ctxt)))
   (define fi (iv from))
   (define si (iv step))
   (define ti (iv to))
   (define len (/ (add1 (- ti fi)) si))
   (maybe-stmts
    (append stmts (list (int-ult-loop (λ (idx) (list (sham-store #`(op-mul #,(sham-integer-literal si elem-type)
                                                                      (op-add #,(sham-integer-literal fi elem-type) #,idx))
                                                            (sham-sequence-index type idx res))))
                                      #`(ui64 0) #`(ui64 #,len))))
    #f)]
  [(sequence-str ctxt str)

   (error 'sham/cryptol "sequence-str ~a" str)])

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
