#lang racket

(require sham/sam/runtime/identifier
         racket/syntax)
(require (for-template (prefix-in ll- sham/llvm/ir)
                       sham/ir))

(require "compiler.rkt"
         "ctxt.rkt"
         "../ast.rkt"
         "utils.rkt"
         "types.rkt")

(provide sham-compiler
         require-syntax
         extra-syntax
         primitive-defs)


(struct res [])
(struct ref res [v])                    ;; stores a pointer to the actual value
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
        [else (error "load-if-needed: needs pointer type: ~a ~a\n" v t)])
      (load-if-ref v)))
(define (maybe-stmts stmts res) (if (empty? stmts) res (stm stmts res)))

(define (cons-box! val blst) (set-box! blst (cons val (unbox blst))))
(define (snoc-box! val blst) (set-box! blst (append (unbox blst) (list val))))

(define (use-ptr-type? cry-type)
  (and
   (or cry-type (error 'sham/cryptol "checking ptr type for unknown ~a" cry-type))
   (or (type-tuple? cry-type) (type-sequence? cry-type))))
(define (add-ptr-type sham-type (ptr-box? #t)) (if ptr-box? #`(ll-type-pointer #,sham-type) sham-type))
(define (to-sham-type cry-type (ptr-box? #f))
  (match cry-type
    [(type-bit) #`i1]
    [(type-tuple ts ...)
     (add-ptr-type #`(ll-type-struct #,@(map (curryr to-sham-type #t) ts)) ptr-box?)]
    [else #`TODO-sham-type]))

(define (sham-function-arg-types up-type p-type ctxt)
  (match-define (list argts ... rst) (get-farg-types up-type))
  `(,@(map (curryr to-sham-type #t) argts) ,(add-ptr-type (to-sham-type rst))))

(define (empty-internal-context) (cons (box '()) (box '())))

(define (add-allocation! ctxt var-name var-type) (snoc-box! #`(#,var-name #,var-type) (car (cc-cctxt ctxt))))
(define (add-allocative! ctxt stmt) (snoc-box! stmt (cdr (cc-cctxt ctxt))))
(define (allocated-vars ictxt) (unbox (car ictxt)))
(define (allocation-stmts ictxt) (unbox (cdr ictxt)))

(define (sham-store! val ptr)
  (match ptr
    [(ref v) #`(store-basic-val! #,val #,(load-if-ref v))]))

(define (add-internal-alloca! ctxt alloc type)
  (match type
    [(type-tuple ts ...)
     (for ([t ts]
           [i (length ts)])
       (when (use-ptr-type? t)
         (define ia (sham-alloca-type! ctxt t))
         ;; peel one ref for both as we are storing the address of sub tuple
         (add-allocative! ctxt (sham-store! (ref-v ia) (ref-v (sham-tuple-index type i alloc))))))]
    [(type-sequence dim t) (error "add-internal-alloca-sequence")]
    [else (void)]))

(define (sham-alloca-type! ctxt type)
  (define alloca-name #`'#,(gensym 'val))
  (define sham-type (to-sham-type type #f))
  (add-allocation! ctxt alloca-name sham-type)
  (define alloc (ref #`(expr-ref #,alloca-name)))
  (add-internal-alloca! ctxt alloc type)
  alloc)

;; just returns the pointer no load
(define (sham-tuple-index type index val)
  (define addr
    #`(op-gep
       #,(load-if-ref
          (match val                    ;; peel one ref as gep itself needs a pointer
            [(ref val) val]))
       (ui32 0)
       (ui32 #,index)))
  (if (use-ptr-type? (maybe-tuple-type-i type index)) (ref (ref addr)) (ref addr)))

(define (compare-for-type type val1 val2)
  (match type
    [(type-bit) #`(op-icmp-eq #,(load-if-ref val1) #,(load-if-ref val2))]
    [(type-tuple ts ...)
     (define tcs
       (for/list ([t ts]
                  [i (length ts)])
         (define (geti val) (sham-tuple-index type i val))
         ;; (define v1 (ref (sham-tuple-index type i val1)))
         ;; (define v2 (ref (sham-tuple-index type i val2)))
         (compare-for-type t (geti val1) (geti val2))))
     (let rec ([c tcs])
       (match c
         [(list c1) c1]
         [(cons fst rst) #`(op-and #,fst #,(rec rst))]
         ['() #`primitive-true]))]
    [else (error 'sham/cryptol "TODO compare for: ~a\n" (pretty-cry type))]))

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
        (with-allocation #,(allocated-vars internal-ctxt)
          (basic-function-body #,@(allocation-stmts internal-ctxt) bodys ...))
        ;; (stmt-expr
        ;;  (make-expr-let `(tvar-args ...) (list tvar-vals ...) (list tvar-types ...)
        ;;                 body-stx
        ;;                 (expr-void)))
        ))]
  [(def-test-results ctxt type)
   (printf "ctxt: ~a ~a\n" ctxt (cc-cctxt ctxt))
   (cons (sham-alloca-type! ctxt type) (sham-alloca-type! ctxt type))]
  [(def-test ctxt name type cr1 r1 cr2 r2)
   (define-values (stmt1 val1) (unwrap-result cr1))
   (define-values (rstmt1 rval1) (unwrap-result r1))
   (define-values (stmt2 val2) (unwrap-result cr2))
   (define-values (rstmt2 rval2) (unwrap-result r2))
   (with-syntax ([name-str (format "~a" (syntax-e (ast-id-stxid name)))])
     #`(with-allocation #,(allocated-vars (cc-cctxt ctxt))
         #,@(allocation-stmts (cc-cctxt ctxt))
         #,@stmt1 #,@rstmt1 #,@stmt2 #,@rstmt2
         (stmt-if #,(compare-for-type type (or val1 rval1) (or val2 rval2))
                  (print-yay name-str)
                  (print-nay name-str))))]

  [(tests ctxt ts)
   #`(make-def-function 'run-tests test-function-type (stmt-block #,@ts (stmt-return-void)))]

  [(function-arg ctxt arg-type index function-type)
   (define stx #`(expr-ref #,index))
   (if (use-ptr-type? arg-type) (ref stx) stx)]
  [(function-result ctxt res-type func-type) (ret #`(expr-ref #,(sub1 (length (get-farg-types func-type)))))]

  [(expr-result ctxt arg-type has-result)
   (printf "expr-result-type: ~a\n" (pretty-cry arg-type))
   (or has-result
       (if (use-ptr-type? arg-type)
           (sham-alloca-type! ctxt arg-type)
           #f))]
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
          [(? syntax? val) (error 'TODO "got stx for app result: ~a ~a\n" rator rands)]
          [#f (error 'TODO "no result, need allocation: ~a ~a" rator rands)]))
      (stm (append stmts rand-stmts (list #`(stmt-expr #,e))) #f)]
     [(env-var name val) (error 'todo "app env-var ~a\n" rator) ;; #`(expr-op #,(ast-id-stxid name) #,@rand-vals)
                         ])]
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
        [else (error 'sham/cryptol "TODO-expr-var: ~a\n" env-value)])))
   (match result
     [(ref ptr) (stm (append stmts (list (sham-store! immediate-value result))) #f)]
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
         [(? syntax?) #`(stmt-expr (op-store! #,val #,(ref-v (sham-tuple-index t i res-name))))])))
   (if res
       (stm (append stmts arg-stmts (filter-not false? (stores res))) #f)
       (error 'sham/sam "tuple-literal needs allocated result to store ~a" args))]
  [(tuple-index ctxt value type index)
   (define-values (stmts r) (unwrap-result value))
   (printf "tuple-index: ~a ~a,~a ~a\n" r type (cc-type ctxt) index)
   (maybe-stmts stmts (sham-tuple-index type index r))]

  [(sequence-str ctxt str) #`(result-string #,str #,(cc-res ctxt))])

(define require-syntax
  #`(
     (require sham/ir
              sham/jit
              sham/md
              sham/cryptol/prelude/sham
              (prefix-in ll- sham/llvm/ir))))

(define extra-syntax
  #`(
     (module+ test
       (define built-sham-env (build-sham-env cryptol-module #:debug #t))
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-llvm-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       (sham-env-optimize-llvm! built-sham-env #:opt-level 2)
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-opt-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       (define built-jit-env (initialize-jit built-sham-env))
       (define test-runner (jit-lookup-function built-jit-env 'run-tests))
       (printf "running-cryptol-tests:\n") (test-runner))))

(define primitive-defs
  #`((ll-def-external 'printf (ll-type-function (ll-type-pointer i8) #t i64))))
