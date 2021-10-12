#lang racket
(require racket/generic
         (for-template racket
                       racket/stxparam))
(require "reqs.rkt"
         (for-template "runtime.rkt"))

(provide (all-defined-out))

;; used for normal variables in pattern
;;   for basic builders this does nothing but keep generate-temporary id along
;;   for syntax builder we swap the syntax in pattern and bind the original in with-syntax
(struct cmplr:pat:var pat:var [orig-stx]
  #:methods gen:stx
  [(define (->syntax pv)
     (match-define (cmplr:pat:var stxid orig-stx) pv)
     (to-syntax stxid))])

;; var which stores type or path
(struct cmplr:pat:tvar cmplr:pat:var [type]
  ;; #:methods gen:stx
  ;; [(define (->syntax cv)
  ;;    (match-define (cmplr:pat:tvar stxid orig-stx type) cv)
  ;;    (to-syntax stxid))]
  )

(struct cmplr:pat:ast:node [arg-stxs ast-node-spec]
  #:methods gen:stx
  [(define (->syntax pan)
     (match-define (cmplr:pat:ast:node arg-stxs ast-node-spec) pan)
     (if arg-stxs
         #`(#,(get-fid (ast:basic-id ast-node-spec)) #,@(to-syntax arg-stxs))
         #`(#,(get-fid (ast:basic-id ast-node-spec)))))])

(struct cmplr:pat:ooo pat:ooo []
  #:methods gen:stx
  [(define (->syntax poo)
     (match-define (cmplr:pat:ooo p k) poo)
     (define ps (to-syntax p))
     (list ps (oook-syntax k ps)))])

(struct cmplr:pat:seq pat:seq []
  #:methods gen:stx
  [(define (->syntax pse)
     (match-define (cmplr:pat:seq ps) pse)
     (seq->syntax ps))])

(struct cmplr:dir [])
(struct cmplr:dir:bind cmplr:dir [var val])
(struct cmplr:dir:bind:val cmplr:dir:bind [])
(struct cmplr-bind-var [stxid depth]
  #:methods gen:stx
  [(define (->syntax bv) (cmplr-bind-var-stxid bv))])

(struct cmplr:ast:match:pat [pat dirs result]
  #:methods gen:stx
  [(define (->syntax mp)
     (match-define (cmplr:ast:match:pat pat dirs result) mp)
     (seq->syntax pat dirs result))])

(struct cmplr:ast:group [id inp args parts]
  #:methods gen:stx
  [(define (->syntax ag)
     (match-define (cmplr:ast:group id inp args parts) ag)
     #`(define (#,(to-syntax id) #,inp #,@(map to-syntax args))
         (syntax-parameterize ([this-ast (make-rename-transformer #'#,inp)])
           (match #,inp
             #,@(map to-syntax parts)
             [else (error 'sham/sam/transform "incorrect value for ~a ~a" '#,id #,inp)]))))])
