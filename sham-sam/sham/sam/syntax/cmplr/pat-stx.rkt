#lang racket
(require racket/generic)
(require "reqs.rkt"
         "pat.rkt")
(provide (all-defined-out))

(struct cmplr:pat:stx:var cmplr:pat:tvar []
  #:methods gen:stx
  [(define/generic to-syntax ->syntax)
   (define (->syntax sv)
     (match-define (cmplr:pat:stx:var id gen-id type) sv)
     #`(~var #,(to-syntax gen-id) #,(to-syntax type)))])

(struct cmplr:stx:stype [tstx state]
  #:methods gen:stx
  [(define (->syntax st)
     (match-define (cmplr:stx:stype tstx state) st)
     (error 'TODO)
     ;; #`(#,tstx #,@(map stx-cls-arg args))
     )])
(struct cmplr:dir:stx:var [stype]
  #:methods gen:stx
  [(define (->syntax d) (error 'TODO))])

;; (struct cmplr:pat:stx:lit pat:dat []
;;   #:methods gen:stx
;;   [(define (->syntax sd)
;;      (match-define (cmplr:pat:stx:lit d) sd)
;;      #`(~datum #,d))])

(struct cmplr:pat:stx:op pat:app []
  #:methods gen:stx
  [(define (->syntax so)
     (match-define (cmplr:pat:stx:op op rands) so)
     #`(#,op #,@(seq->syntax rands)))])

;; (struct cmplr:pat:stx pat:dat []
;;   #:methods gen:stx
;;   [(define (->syntax st) (pat:dat-v st))])

(struct cmplr:pat:stx:seq cmplr:pat:seq [paren-shape]
  #:methods gen:stx
  [(define (->syntax sv)
     (match-define (cmplr:pat:stx:seq ps shape) sv)
     #`(#,@(seq->syntax ps)))])

(struct cmplr:pat:stx:vec cmplr:pat:stx:seq []
  #:methods gen:stx
  [(define (->syntax sv)
     (match-define (cmplr:pat:stx:vec ps shape) sv)
     #`#(#,@(stx-seq ps)))])



;; (struct stx-cls-attr-val [var id]
;;   #:methods gen:stx
;;   [(define/generic to-syntax ->syntax)
;;    (define (->syntax sca)
;;      (match-define (stx-cls-attr-val var id) sca)
;;      (define var-stx (to-syntax var))
;;      (if id
;;          #`(attribute #,(format-id var-stx "~a.~a" var-stx (to-syntax id)))
;;          #`(attribute #,var)))])

;; (struct stx-cls-with-var [id depth]
;;   #:methods gen:stx
;;   [(define/generic to-syntax ->syntax)
;;    (define (->syntax sv)
;;      (match-define (stx-cls-with-var id depth) sv)
;;      (let rec ([d depth])
;;        (match d
;;          [#f (to-syntax id)]
;;          [(cons (cons mn mx) rst)
;;           #`(#,(rec rst) (... ...))])))])

;; syntax-class directive ; #:with/#:when/#:attr ...
(struct stx-cls-dir [kwrd vals]
  #:methods gen:stx
  [(define (->syntax scp)
     (match-define (stx-cls-dir kwrd vals) scp)
     (cons (to-syntax kwrd) (to-syntax vals)))])

;; (struct stx-class-var-operator []
;;   #:methods gen:cmplr-node-operator
;;   [(define (cmplr-operator-node-syntax op stxs)
;;      (match-define (cmplr:node:case pat bodys) stxs)
;;      ;; TODO
;;      (cmplr:node:stx-cls-pat pat bodys))])


(define (stx-cls-arg arg)
  #`(#,(->syntax-keyword arg) #,arg))
