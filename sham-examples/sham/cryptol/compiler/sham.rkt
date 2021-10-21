#lang racket

(require sham/sam/runtime/identifier)
(require (for-template (prefix-in ll- sham/llvm/ir)
                       sham/ir))

(require "compiler.rkt"
         "ctxt.rkt"
         "../ast.rkt"
         "types.rkt")
(provide sham-compiler)
(define (cons-box! val blst)
  (set-box! blst (cons val (unbox blst))))
(define (to-sham-type cry-type)
  (match cry-type
    [(type-bit) #`ll-i1]
    [else #`TODO-sham-type]))
(define (add-ptr-type sham-type) #`(ll-type-pointer #,sham-type))

(define (sham-function-arg-types up-type p-type ctxt)
  (match-define (list argts ... rst) (get-farg-types up-type))
  `(,@(map to-sham-type argts) ,(add-ptr-type (to-sham-type rst))))
(define (sham-alloca-type ctxt type)
  (define alloca-name (gensym 'val))
  (define sham-type (to-sham-type type))
  (define let-id-stx #`(#,alloca-name (op-alloca #,sham-type) #,(add-ptr-type sham-type)))
  (cons-box! let-id-stx (cc-cctxt ctxt))
  alloca-name)

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
        'name-stxid (list arg-types ...) #f i64
        (expr-let #,(unbox internal-ctxt)
                  (stmt-block body-stx
                              (stmt-return (ui64 0)))
                  (expr-void))
        ;; (stmt-expr
        ;;  (make-expr-let `(tvar-args ...) (list tvar-vals ...) (list tvar-types ...)
        ;;                 body-stx
        ;;                 (expr-void)))
        ))]
  [(def-test-results ctxt type)
   (printf "ctxt: ~a ~a\n" ctxt (cc-cctxt ctxt))
   (cons (sham-alloca-type ctxt type) (sham-alloca-type ctxt type))]
  [(def-test ctxt name type val1 val2)
   (with-syntax ([name-str (format "~a" (syntax-e (ast-id-stxid name)))])
     #`(stmt-if (compare-for-type #,type #,val1 #,val2) (print-yay name-str) (print-nay name-str)))]

  [(tests ctxt ts)
   #`(make-def-function 'run-tests (list) #f (ll-type-void)
                        (stmt-block #,@ts
                                    (stmt-return-void)))]

  [(function-arg ctxt arg-type index function-type) #`(expr-ref #,index)]
  [(function-result ctxt res-type func-type) #`(expr-ref #,(sub1 (length (get-farg-types func-type))))]

  [(expr-app ctxt rator rands)
   (printf "sham-expr-app: ~a ~a\n" rator rands)
   (match rator
     [(env-special-var name val type oname otype pargs)
      (define result-alloca #`result-alloca)
      #`(expr-op #,(ast-id-stxid name) #,@rands #,result-alloca)]
     [(env-var name val) #`(expr-op #,(ast-id-stxid name) #,@rands)])]
  [(expr-bind ctxt value) value]
  [(expr-var ctxt name env-value)
   (define result (cc-res ctxt))
   (define name-stx (ast-id-stxid name))
   (match (syntax-e name-stx)
     ['true #`(ui1 1)]
     ['false #`(ui1 0)]
     [else (if result #`(op-store! #,(env-var-val env-value) #,result) #`(expr-ref #,name))])]

  [(tuple-index ctxt value type index) #`(op-gep #,value (ui64 0) (ui64 #,index))]


  [(sequence-str ctxt str) #`(result-string #,str #,(cc-res ctxt))])
