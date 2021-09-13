#lang racket
(require racket/generic)
(require "generics.rkt")
(provide (all-defined-out))

(define (stx-seq . stxs)
  (define fs (->syntax (flatten stxs)))
  #`(#,@fs))

(struct stx:def [vars vals]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax ds)
     (match-define (stx:def vars vals) ds)
     (cond
       [(list? vars)
        #`(define-values #,(to-syntax vars) #,(to-syntax vals))]
       [(not (list? vars))
        #`(define #,(to-syntax vars) #,(to-syntax vals))]
       [else (error 'sham/sam/stx "unknown vars and vals in define: ~a ~a" vars vals)]))])

(struct stx:local-def [type defs body]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax sld)
     (match-define (stx:local-def type defs body) sld)
     (stx-seq
      (to-syntax type)
      (apply stx-seq
             (for/list [(def defs)]
               ;; TODO check for values
               (match def
                 [(stx:def vars vals) (stx-seq (to-syntax vars) (to-syntax vals))]
                 [(cons vars vals) (stx-seq (to-syntax vars) (to-syntax vals))]
                 [else (error 'sham/sam/stx "unknown definition for local def ~a" def)])))
      (to-syntax body)))])

(struct stx:lam [args body]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax sl)
     (match-define (stx:lam args body) sl)
     #`(λ #,(to-syntax args) #,(to-syntax body)))])

(struct stx:app [op rands]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax sa)
     (match-define (stx:app op rands) sa)
     #`(#,(to-syntax op) . #,(stx-seq rands)))])

(struct stx:match [inp cases]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax sm)
     (match-define (stx:match inp cases) sm)
     #`(match #,(to-syntax inp)
         #,@(to-syntax (flatten cases))))])
