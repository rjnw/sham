#lang racket

(require sham/sam/runtime/identifier)
(require "compiler.rkt"
         "ctxt.rkt"
         "../ast.rkt"
         "types.rkt")
(provide sham-compiler)

(define (to-sham-type cry-type)
  (match cry-type
    [(type-bit) #`i1]
    [else #`TODO-sham-type]))

(define (sham-function-arg-types up-type p-type ctxt)
  (define argts (get-farg-types up-type))
  (map to-sham-type argts))

(define-compiler sham-compiler
  [(def-val ctxt orig-name new-name up-type orig-type pargs-evs cval)
   (with-syntax
     ([name-stxid (ast-id-stxid new-name)]
      [(arg-types ...) (sham-function-arg-types up-type orig-type ctxt)]
      [(tvar-args ...) (map ast-id-stxid (map env-var-name pargs-evs))]
      [(tvar-vals ...) (map env-var-val pargs-evs)]
      [(tvar-types ...) (map (const #`i64) pargs-evs)]
      [body-stx cval])
     #`(make-def-function
        'name-stxid (list arg-types ...) #f i64
        (stmt-expr
         (make-expr-let `(tvar-args ...) (list tvar-vals ...) (list tvar-types ...)
                        body-stx
                        (expr-void)))))]

  [(tuple-index ctxt value type index)
   #`(op-gep #,value (ui64 0) (ui64 #,index))]

  [(sequence-str ctxt str) #`(result-string #,str #,(cc-res ctxt))])
