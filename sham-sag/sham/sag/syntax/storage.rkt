#lang racket

(require (for-template racket))
(require "spec.rkt"
         "pattern.rkt")
(provide group-arg-storage
         node-args-storage
         from-node-storage)

(define (group-arg-storage gargs)
  #`(vector #,@gargs))

(define (node-args-storage nargs pat)
  (define (fsingle i p) i)
  (define (fdatum d p) #f)
  (define (fmultiple ss p)
    #`(vector #,@(for/list ([s ss] #:when s) s)))
  (define (frepeat s p) s)
  (rec-pattern pat fsingle fdatum fmultiple frepeat))

(define ((from-node-storage arg pat) val)
  (define (generate-access path)
    (match path
      [`() (values val 0)]
      [`(single ,c ,i ,p) (generate-access p)]
      [`(multiple ,idx ,s ,p)
       (define (datum-until ds ci)
         (cond [(>= ci idx) ds]
               [(ast:pat:datum? (vector-ref s ci)) (datum-until (add1 ds) (add1 ci))]
               [else (datum-until ds (add1 ci))]))
       (define actual-idx (- idx (datum-until 0 0)))
       (define-values (val depth) (generate-access p))
       (if (> depth 0)
           (values #`(map (curryr vector-ref #,actual-idx) #,val) (sub1 depth))
           (values #`(vector-ref #,val #,actual-idx) depth))]
      [`(repeat ,s ,k ,p)
       (define-values (val depth) (generate-access p))
       (values val (add1 depth))]))
  (define pat-path (find-arg pat (car (ast:basic-id-tpair arg))))
  (define-values (stx _) (generate-access (cdr pat-path)))
  stx)

(module+ test
  (require (submod "pattern.rkt" test))
  (printf "storage:\n")
  (define aa (ast:node:arg (cons a a) a #f '()))
  (define ba (ast:node:arg (cons b b) b #f '()))
  ((from-node-storage aa p1) f)
  (define pp (mlt (dat 'lambda) (mlt (rpt (mlt (sng a) (sng b))) (sng c))))
  ((from-node-storage aa pp) f)
  ((from-node-storage ba pp) f)
  (define p2 (mlt (dat 'a) (sng a) (dat 'b) (dat 'c) (mlt (sng b) (sng c))))
  ((from-node-storage aa p2) f)
  ((from-node-storage ba p2) f))
