#lang racket

(require (for-template racket))
(require "spec.rkt"
         "pattern.rkt"
         "private/utils.rkt")
(provide group-arg-storage
         node-args-storage
         from-node-storage)

(define (group-arg-storage gargs)
  #`(vector #,@gargs))

(define (node-args-storage nargs pat)
  (define (fsingle i p)
    (get-fid
     (ast:basic-id
      (findf (λ (a) (equal? i (get-oid (ast:basic-id a)))) nargs))))
  (define (fdatum d p) #f)
  (define (fmultiple ss p)
    (cons #`vector (for/list ([s ss] #:when s) s)))
  (define (frepeat s p)
    (if (list? s) (cons #`map s) s))
  (define (flat-syn s)
    (match s
      [(list ss ...)
       #`(#,@(map flat-syn ss))]
      [(? syntax?) s]
      [#f (match pat
            [(ast:pat:datum d) d])]))
  (flat-syn (rec-pattern pat fsingle fdatum fmultiple frepeat)))

(define ((generate-access path) val)
  (define-values (stx _)
    (let rec ([p path])
      (match p
        [`() (values val 0)]
        [`(in-multiple ,idx ,ps ,ppath)
         (define datum-until (count-until ps (λ (v i) (ast:pat:datum? v)) (λ (v i) (equal? i idx))))
         (define without-datum-idx (- idx datum-until))
         (define-values (val depth) (rec ppath))
         (if (> depth 0)
             (values #`(map (curryr vector-ref #,without-datum-idx) #,val) (sub1 depth))
             (values #`(vector-ref #,val #,without-datum-idx) depth))]
        [`(in-repeat ,p ,k ,ppath)
         (define-values (val depth) (rec ppath))
         (values val (add1 depth))])))
  stx)

;; checks whether given identifier matches with the given single pattern
(define (arg-pattern? id p)
  (match p
    [(ast:pat:single c i) (equal? i id)]
    [else #f]))

;; returns pattern path for the sub-pattern matching the given argument in full pattern
(define (arg-path arg pat)
  (cond
    [(find-pattern pat (curry arg-pattern? (get-oid (ast:basic-id arg)))) => cdr]
    [else #f]))

(define (from-node-storage arg pat)
  (cond [(arg-path arg pat) => generate-access]
        [else (error 'sham/sam/internal "could not find arg in pattern ~a ~a" arg pat)]))

(module+ test
  (require rackunit)
  (require (submod "pattern.rkt" test))
  (define (node-arg i) (ast:node:arg `((0 . ,i) (f . ,i)) '() '()))
  (define aa (node-arg a))
  (define ba (node-arg b))
  (define ca (node-arg c))
  (define args (list aa ba ca))
  (check-equal? (syntax->datum ((from-node-storage aa p1) f))
                `f)
  (define pp (mlt (dat 'lambda) (mlt (rpt (mlt (sng a) (sng b))) (sng c))))
  (check-equal? (syntax->datum ((from-node-storage aa pp) f))
                `(map (curryr vector-ref 0) (vector-ref (vector-ref f 0) 0)))
  (check-equal? (syntax->datum ((from-node-storage ba pp) f))
                `(map (curryr vector-ref 1) (vector-ref (vector-ref f 0) 0)))
  (define p2 (mlt (dat 'a) (sng a) (dat 'b) (dat 'c) (mlt (sng b) (sng c))))
  (check-equal? (syntax->datum ((from-node-storage aa p2) f))
                `(vector-ref f 0))
  (check-equal? (syntax->datum ((from-node-storage ba p2) f))
                `(vector-ref (vector-ref f 1) 0))

  (check-equal? (syntax->datum (node-args-storage args plam))
                `(vector (vector (map vector a b)) c)))
