#lang racket

(require racket/syntax
         (for-template racket syntax/parse))

(require "reqs.rkt"
         "pat.rkt")

(provide (all-defined-out))

(struct cmplr:pat:stx:var cmplr:pat:tvar []
  #:methods gen:stx
  [(define (->syntax sv)
     (match-define (cmplr:pat:stx:var id orig-id type) sv)
     (if type
         #`(~var #,(to-syntax id) #,(to-syntax type))
         (to-syntax id)))])

(struct cmplr:pat:stx:dat pat:dat []
  #:methods gen:stx
  [(define (->syntax sd)
     (match-define (cmplr:pat:stx:dat d) sd)
     #`(~datum #,d))])

(struct cmplr:pat:stx:op pat:app []
  #:methods gen:stx
  [(define (->syntax so)
     (match-define (cmplr:pat:stx:op op rands) so)
     #`(#,op #,@(seq->syntax rands)))])

(struct cmplr:pat:stx:seq cmplr:pat:seq [paren-shape]
  #:methods gen:stx
  [(define (->syntax sv)
     (match-define (cmplr:pat:stx:seq ps shape) sv)
     #`(#,@(seq->syntax ps)))])

(struct cmplr:pat:stx:vec cmplr:pat:stx:seq []
  #:methods gen:stx
  [(define (->syntax sv)
     (match-define (cmplr:pat:stx:vec ps shape) sv)
     #`#(#,@(seq->syntax ps)))])

;; (struct cmplr:dir:bind:stx cmplr:dir:bind [])

(define (combine-binds-with-let dirs body)
  (define (create-let-bind dir)
    (match dir
      [(cmplr:dir:bind var val)
       #`[#,(to-syntax var) #,(to-syntax val)]]))
  (define let-binds (map create-let-bind dirs))
  #`(let #,let-binds #,(to-syntax body)))

(define (combine-binds-with-syntax dirs body)
  (define (create-with-bind dir)
    (define (var-stxid var)
      (match var
        [(? identifier?) var]
        [(cmplr-bind-var stxid depth)
         (to-syntax (stx-cls-with-var stxid depth))]))
    (match dir
      [(cmplr:dir:bind var val)
       (printf "vv: ~a ~a\n" var val)
       #`(#,(var-stxid var) #,(to-syntax val))]))
  (define with-binds (map create-with-bind dirs))
  #`(with-syntax #,with-binds #,(to-syntax body)))

(struct cmplr:dir:stx [])
;; syntax-class directive ; #:with/#:when/#:attr ...
(struct cmplr:dir:stx:kwrd [kwrd vals]
  #:methods gen:stx
  [(define (->syntax scp)
     (match-define (cmplr:dir:stx:kwrd kwrd vals) scp)
     (cons (to-syntax kwrd) (to-syntax vals)))])

(struct cmplr:dir:stx:with cmplr:dir:stx [pat-stx val-stx]
  #:methods gen:stx
  [(define (->syntax pw)
     (match-define (cmplr:dir:stx:with pat-stx val-stx) pw)
     (list #'#:with (to-syntax pat-stx) (to-syntax val-stx)))])

(struct cmplr:dir:stx:attr [attr-stx val]
  #:methods gen:stx
  [(define (->syntax pa)
     (match-define (cmplr:dir:stx:attr attr-stx val-stx) pa)
     (list #'#:attr (to-syntax attr-stx) (to-syntax val-stx)))])

(struct cmplr:stx:class:pat [pat dirs]
  #:methods gen:stx
  [(define (->syntax cp)
     (match-define (cmplr:stx:class:pat pat dirs) cp)
     (seq->syntax #'pattern pat dirs))])

(struct cmplr:stx:class [id parts splicing?]
  #:methods gen:stx
  [(define (->syntax sc)
     (match-define (cmplr:stx:class id parts splicing?) sc)
     (define definer (if splicing? #`define-splicing-syntax-class #`define-syntax-class))
     #`(#,definer #,(to-syntax id) #,@(map to-syntax parts)))])

(struct stx-cls-attr-val [var id]
  #:methods gen:stx
  [(define (->syntax sca)
     (match-define (stx-cls-attr-val var attr) sca)
     (define var-stx (to-syntax var))
     (define attr-stx (and attr (to-syntax attr)))
     (if attr-stx
         #`(attribute #,(format-id var-stx "~a.~a" var-stx attr-stx))
         #`(attribute #,var)))])

(struct stx-cls-with-var [id depth]
  #:methods gen:stx
  [(define (->syntax sv)
     (match-define (stx-cls-with-var id depth) sv)
     (let rec ([d depth])
       (match d
         [#f (to-syntax id)]
         [(cons (cons mn mx) rst)
          #`(#,(rec rst) (... ...))])))])

(define (internal-class-args-stx args)
  (append-map (λ (arg) (list (->syntax-keyword arg) arg)) args))
