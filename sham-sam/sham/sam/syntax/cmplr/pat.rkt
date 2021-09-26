#lang racket
(require racket/generic)
(require "reqs.rkt")
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
     (match-define (cmplr:pat:ast:node op rands) pan)
     (printf "ast-node: ~a\n" rands)
     (error 'sham/sam/TODO "cmplr:pat:ast:node")
     (define rands-stx
       (match rands
         [(list (cmplr:pat:seq ps))
          (to-syntax (flatten ps))]
         [else (to-syntax rands)]))
     #`(#,(get-fid (ast:basic-id op)) #,@rands-stx))])

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
