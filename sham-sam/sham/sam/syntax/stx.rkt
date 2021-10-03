#lang racket
(require racket/generic
         (for-template racket racket/syntax))

(require "generics.rkt")
(provide (all-defined-out))

(define to-syntax ->syntax)

;; converts a list of stx into an actual list syntax
(define (seq->syntax . stxs)
  (define fs (flatten (->syntax (flatten stxs))))
  #`(#,@fs))

(define (stx-seq . stxs) (stx:seq (flatten stxs)))

(struct stx:seq [stxs]
  #:methods gen:stx
  [(define (->syntax ss) (seq->syntax (stx:seq-stxs ss)))])

(struct stx:forced-seq [stx]
  #:methods gen:stx
  [(define (->syntax fs)
     (syntax-e (to-syntax (stx:forced-seq-stx fs))))])

(define (stx-define-values vars vals)
  #`(define-values #,(to-syntax vars) #,(to-syntax vals)))

(struct stx:def [vars vals]
  #:methods gen:stx
  [(define (->syntax ds)
     (match-define (stx:def vars vals) ds)
     (cond
       [(list? vars)
        #`(define-values #,(to-syntax vars) #,(to-syntax vals))]
       [(not (list? vars))
        #`(define #,(to-syntax vars) #,(to-syntax vals))]
       [else (error 'sham/sam/stx "unknown vars and vals in define: ~a ~a" vars vals)]))])

(struct stx:local-def [type defs body]
  #:methods gen:stx
  [(define (->syntax sld)
     (match-define (stx:local-def type defs body) sld)
     (seq->syntax
      (to-syntax type)
      (apply seq->syntax
             (for/list [(def defs)]
               ;; TODO check for values
               (match def
                 [(stx:def vars vals) (seq->syntax (to-syntax vars) (to-syntax vals))]
                 [(cons vars vals) (seq->syntax (to-syntax vars) (to-syntax vals))]
                 [else (error 'sham/sam/stx "unknown definition for local def ~a" def)])))
      (to-syntax body)))])

(struct stx:lam [args body]
  #:methods gen:stx
  [(define (->syntax sl)
     (match-define (stx:lam args body) sl)
     #`(λ #,(to-syntax args) #,(to-syntax body)))])

(struct stx:app [op rands]
  #:methods gen:stx
  [(define (->syntax sa)
     (match-define (stx:app op rands) sa)
     #`(#,(to-syntax op) . #,(seq->syntax rands)))])

(struct stx:match [inp cases]
  #:methods gen:stx
  [(define (->syntax sm)
     (match-define (stx:match inp cases) sm)
     #`(match #,(to-syntax inp) #,@(map seq->syntax cases)))])

(struct stx:qs [s]
  #:methods gen:stx
  [(define (->syntax qs)
     (match-define (stx:qs s) qs)
     #`(#,#'quasisyntax #,s))])

(struct stx:uns [s]
  #:methods gen:stx
  [(define (->syntax qs)
     (match-define (stx:uns s) qs)
     #`(#,#'unsyntax #,s))])
