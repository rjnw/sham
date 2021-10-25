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
(define (cons-box! val blst)
  (set-box! blst (cons val (unbox blst))))
(define (to-sham-type cry-type)
  (match cry-type
    [(type-bit) #`i1]
    [else #`TODO-sham-type]))
(define (add-ptr-type sham-type) #`(ll-type-pointer #,sham-type))

(define (sham-function-arg-types up-type p-type ctxt)
  (match-define (list argts ... rst) (get-farg-types up-type))
  `(,@(map to-sham-type argts) ,(add-ptr-type (to-sham-type rst))))
(define (sham-alloca-type! ctxt type)
  (define alloca-name #`'#,(gensym 'val))
  (define sham-type (to-sham-type type))
  (define let-vars #`(#,alloca-name #,sham-type
                             ;; (op-alloca (expr-etype #,sham-type))
                             ;; #,(add-ptr-type sham-type)
                             ))
  (cons-box! let-vars (cc-cctxt ctxt))
  #`(expr-ref #,alloca-name))
(define (compare-for-type type val1 val2)
  (match type
    [(type-bit) #`(op-icmp-eq (op-load #,val1) (op-load #,val2))]
    [else (error 'sham/cryptol "TODO compare for: ~a\n" (pretty-cry type))]))

(define-compiler sham-compiler
  [(internal-def-context name type ctxt)
   (box '())]
  [(def-val ctxt orig-name new-name up-type orig-type pargs-evs cval internal-ctxt)
   (printf "def-val: ~a\n" internal-ctxt)
   (with-syntax
     ([name-stxid (ast-id-stxid new-name)]
      [(arg-types ...) (sham-function-arg-types up-type orig-type ctxt)]
      [(tvar-args ...) (map ast-id-stxid (map env-var-name pargs-evs))] ;; TODO only need type vars with kind int
      [(tvar-vals ...) (map env-var-val pargs-evs)]
      [(tvar-types ...) (map (const #`ll-i64) pargs-evs)]
      [body-stx cval])
     #`(make-def-function
        'name-stxid (basic-function-type arg-types ...)
        (with-allocation #,(unbox internal-ctxt) (basic-function-body body-stx))
        ;; (stmt-expr
        ;;  (make-expr-let `(tvar-args ...) (list tvar-vals ...) (list tvar-types ...)
        ;;                 body-stx
        ;;                 (expr-void)))
        ))]
  [(def-test-results ctxt type)
   (printf "ctxt: ~a ~a\n" ctxt (cc-cctxt ctxt))
   (cons (sham-alloca-type! ctxt type) (sham-alloca-type! ctxt type))]
  [(def-test ctxt name type stmt1 val1 stmt2 val2)
   (with-syntax ([name-str (format "~a" (syntax-e (ast-id-stxid name)))])
     #`(with-allocation #,(unbox (cc-cctxt ctxt))
         #,stmt1
         #,stmt2
         (stmt-if #,(compare-for-type type val1 val2)
                  (print-yay name-str)
                  (print-nay name-str))))]

  [(tests ctxt ts)
   #`(make-def-function 'run-tests test-function-type
                        (stmt-block #,@ts (stmt-return-void)))]

  [(function-arg ctxt arg-type index function-type) #`(expr-ref #,index)]
  [(function-result ctxt res-type func-type) #`(expr-ref #,(sub1 (length (get-farg-types func-type))))]

  [(expr-app ctxt rator rands)
   (printf "sham-expr-app: ~a ~a\n" rator rands)
   (match rator
     [(env-special-var name val type oname otype pargs)
      (define result-val (cc-res ctxt))
      (if result-val
          #`(stmt-expr (expr-op '#,(ast-id-stxid name) #,@rands #,result-val))
          (error 'TODO "no result, need allocation: ~a ~a" rator rands))]
     [(env-var name val) #`(expr-op #,(ast-id-stxid name) #,@rands)])]
  [(expr-bind ctxt value) value]
  [(expr-var ctxt name env-value)
   (printf "expr-var: ~a ~a\n" name env-value)
   (define result (cc-res ctxt))
   (define name-stx (ast-id-stxid name))
   (define immediate-value
     (match env-value
       [(env-primitive-var name type) (format-id #f "primitive-~a" name-stx)]
       ;; ['true #`bit-true]
       ;; ['false #`bit-false]
       [(env-var name val) val]
       [else (error 'sham/cryptol "TODO-expr-var: ~a\n" env-value)]))
   (if result #`(store-basic-val! #,immediate-value #,result) immediate-value)]

  [(tuple-index ctxt value type index) #`(op-gep #,value (ui64 0) (ui64 #,index))]


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
     (define built-sham-env (build-sham-env cryptol-module))
     (sham-verify-llvm-ir built-sham-env)
     (sham-env-optimize-llvm! built-sham-env #:opt-level 2)
     (sham-print-llvm-ir built-sham-env)
     (sham-verify-llvm-ir built-sham-env)
     (define built-jit-env (initialize-jit built-sham-env))
     (define test-runner (jit-lookup-function built-jit-env 'run-tests))
     (module+ test (printf "running-cryptol-tests:\n") (test-runner))))

(define primitive-defs
  #`((ll-def-external 'printf (ll-type-function (ll-type-pointer i8) #t i64))))
