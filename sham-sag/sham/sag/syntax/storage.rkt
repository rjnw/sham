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
    #`(vector #,@(filter-not false? ss)))
  (define (frepeat s p)
    s)
  (rec-pattern pat fsingle fdatum fmultiple frepeat))

(define ((from-node-storage arg pat) val)
  (define (generate-access path v rdepth)
    (match path
      [`(single ,i ,p) v]
      [`(multiple ,s ,i ,p)
       (generate-access s (if (> rdepth 0)
                              #`(map (curryr vector-ref #,i) #,v)
                              #`(vector-ref #,v #,i))
                        (max 0 (sub1 rdepth)))]
      [`(repeat ,s ,p) (generate-access s v (add1 rdepth))]))
  (generate-access (find-arg pat (car (ast:basic-id-tpair arg))) val 0))

(module+ test
  (define a-sym #`a)
  (define a-arg (ast:node:arg (cons a-sym #f) a-sym #f #f))
  (define p1 (ast:pat:multiple (list (ast:pat:single #f a-sym))))
  ((from-node-storage a-arg p1) #'args)
  (define p2 (ast:pat:multiple (list (ast:pat:repeat (ast:pat:single #f a-sym) (list #f)))))
  ((from-node-storage a-arg p2) #'args)
  (define p3
    (ast:pat:multiple
     (list
      (ast:pat:repeat (ast:pat:multiple (list (ast:pat:single #f a-sym) (ast:pat:single #f #`0)))
                      (list #f)))))
  ;; storage := (vector/c (list/c (vector/c a-sym 0)))
  ((from-node-storage a-arg p3) #'args) ;; (map (curryr vector-ref 0) (vector-ref args 0))
  (define pt
    (ast:pat:multiple
     (list
      (ast:pat:datum `letrec)
      (ast:pat:multiple
       (list
        (ast:pat:repeat
         (ast:pat:multiple
          (list (ast:pat:single #f a-sym) (ast:pat:single #f #'vals)))
         (list #f))))
      (ast:pat:single #f #'e))))
  ((from-node-storage a-arg pt) #'args))
