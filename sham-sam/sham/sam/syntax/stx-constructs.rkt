#lang racket
(require racket/generic)
(require "generics.rkt")
(provide (all-defined-out))

(define (stx-seq . stxs)
  (define fs (flatten stxs))
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
      (for/list [(def defs)]
        ;; TODO check for values
        (match def
          [(stx:def vars vals) (stx-seq (to-syntax vars) (to-syntax vals))]
          [(cons vars vals) (stx-seq (to-syntax vars) (to-syntax vals))]
          [else (error 'sham/sam/stx "unknown definition for local def ~a" def)]))
      (to-syntax body)))])
