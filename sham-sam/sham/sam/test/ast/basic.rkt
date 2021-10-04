#lang racket

(require sham/sam/ast
         sham/sam/custom
         sham/sam/runtime
         (for-syntax sham/sam/syntax/spec))

(provide (all-defined-out))

(define-ast basic
  (expr
   [zer]
   [seq (a:expr b:expr c:expr)]
   [rep (a:expr ...)]
   [sr1 (a:expr ... b:expr)]
   [sr2 (a:expr (b:expr ...))]
   [dr2 (a ((b c) ...) ... d)])
  #:format (#f - #f - -)
  )

(module+ test
  (require rackunit)
  (define z (zer))
  (check-true
   (match z
     [(zer) #t]
     [else #f]))

  (define s-s1 (seq 1 2 3))
  (define m-s1 (make-seq 1 2 3))
  (check-equal? (seq-a s-s1) 1)
  (check-equal? (seq-b s-s1) 2)
  (check-equal? (seq-c s-s1) 3)

  (check-equal? (seq-a m-s1) 1)
  (check-equal? (seq-b m-s1) 2)
  (check-equal? (seq-c m-s1) 3)

  (define s-r1 (rep 1 2 3))
  (define m-r1 (make-rep `(1 2 3)))
  (check-equal? (rep-a s-r1) (rep-a m-r1))

  (define s-dr2 (dr2 'a (('b1 'c1) ('b2 'c2)) (('bb1 'cc1)) 'd))
  (define m-dr2 (make-dr2 'a `((b1 b2) (bb1)) `((c1 c2) (cc1)) 'd))
  (check-equal? (dr2-b s-dr2) (dr2-b m-dr2))
  (check-equal? (ast:term-args s-dr2) (ast:term-args m-dr2)))
