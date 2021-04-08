#lang racket

(require (for-template racket))
(require "spec.rkt")
(provide ;; fold-with-pattern
         rec-pattern
         find-arg)

(define (ooo p)
  ;; corresponds to `ooo` in racket's match grammar
  ;; matches for ..., ___, ..k, __k
  ;; syntax? ->  (or/c false? (list/c (or/c false? integer?)))
  (define (sooo s)
    (match (regexp-match #px"^((__|\\.\\.)(_|\\.|[[:digit:]]+))$" s)
      [#f #f]
      [(list a b c k) (list (string->number k))]))
  (and (identifier? p)
       (sooo (symbol->string (syntax->datum p)))))

(define (remove-datum pts) (filter-not ast:pat:datum? pts))
(define (kid s c) s)                  ;; todo check c is fully consumed
(define (consume c (n 1))
  (match c
    [(cons h t) (cons (if h (- h n) h) t)]
    [null null]))
(define (is-consumed? c)
  (match c
    [(list h t) (and (is-consumed? h) (is-consumed? t))]
    [null #t]
    [#f #t]
    [0 #t]
    [else #f]))

(define (pattern-path/c val/c)
  (flat-rec-contract pattern-path/c
                     (list/c)
                     (list/c 'in-multiple natural-number/c (listof ast:pat/c) pattern-path/c)
                     (list/c 'at-multiple (-> val/c natural-number/c val/c) pattern-path/c)
                     (list/c 'in-repeat ast:pat/c (or/c false/c natural-number/c) pattern-path/c)
                     (list/c 'at-repeat (-> val/c (or/c false/c natural-number/c) val/c))))

(define (pretty-path path)
  (match path
    [`() '()]
    [`(in-multiple ,i ,pats ,ppath) `(in-multiple ,i ,(map pretty-pattern (vector->list pats)) ,(pretty-path ppath))]
    [`(at-multiple ,frec ,ppath) `(@-multiple ,(pretty-path ppath))]
    [`(in-repeat ,pat ,k ,ppath) `(in-repeat ,(pretty-pattern pat) ,k ,(pretty-path ppath))]
    [`(at-repeat ,frec ,ppath) `(@-repeat ,(pretty-path ppath))]))

;; f := val/c ast:pat/c pattern-path/c -> val/c
;;  folds over pattern along with a zipper containing the position of current pattern
;;  f is also polymorphic for pre and post multiple values
;;    repeat pattern is done with a special path containing a recursive function to perform
;;    the fold differently
(define (pattern-with-zipper f val pat)
  (define (go path val pat)
    (match pat
      [(ast:pat:single chk id) (f val pat path)]
      [(ast:pat:datum d) (f val pat path)]
      [(ast:pat:multiple ps)
       (define (frec cval i)
         (go `(in-multiple ,i ,ps ,path) cval (vector-ref ps i)))
       (f val pat `(at-multiple ,frec ,path))]
      [(ast:pat:repeat p k)
       (define (frec cval nk)
         (go `(in-repeat ,p ,nk ,path) cval p))
       (f val pat `(at-repeat ,frec ,path))]))
  (go null val pat))

(define (default-single-format ss pat) ss)
(define (default-datum-format ss pat) #f)
(define (default-multiple-format ss pat) #`(vector . #,ss))
(define (default-repeat-format ss pat) #`(list . #,ss))

;; TODO why this? why not generate a syntax-parse for pat, write a more conventional pattern matcher
;;  this stx -> match pattern for list/vector ;; or anything else
;; fold over untyped with the interpretation of syntax given by the pattern
;; TODO change ostx to actual stack instead of functions to enable backtracking

(define (parse-with-pattern pat stx)
  (printf "fwp: ~a" stx) (newline) (pretty-print (pretty-pattern pat)) (newline)
  (struct stack [input output] #:prefab)
  (define (f sc pat path)
    (match-define (stack is os) sc)
    (match pat
      [(ast:pat:single chk id)
       (printf "fs: ~a\n" is) (pretty-print (pretty-path path))
       (match (ooo (car is))
         [(list n) (stack (cdr is) (cons `(ooo ,(car is) ,n ,pat) os))]
         [else (stack (cdr is) (cons `(single ,(car is) ,pat) os))])]
      [(ast:pat:datum val)
       (cond [(and (identifier? (car is)) (equal? (syntax->datum (car is)) val))
              (stack (cdr is) (cons `(datum ,(car is) ,pat) os))]
             [else (stack is os)])]
      [(ast:pat:multiple pms)
       (printf "fm: ~a\n" is) (pretty-print (pretty-path path))
       (match* ((ooo (car is)) path)
         [((list n) `(at-multiple ,frec ,prev-path))
          (stack (cdr is) (cons `(ooo ,(car is) ,n ,pat) os))]
         [('#f `(at-multiple ,frec ,prev-path))
          (printf "fm: ~a ~a ~a\n" is os (pretty-path prev-path))
          (match-define (stack nis nos)
            (let rec ([cval (stack (syntax-e (car is)) '())]
                       [i 0])
              (cond [(equal? i (vector-length pms)) cval]
                    [else (rec (frec cval i) (add1 i))]))
            ;; (for/fold ([cval (sctxt (syntax-e (car is)) '() kl)]) ;; TODO use general recursion instead of fold
            ;;           ([i (vector-length pms)])
            ;;   ;; TODO pop from repeat stack if nis empty
            ;;   (frec cval i))
            )
          (unless (empty? nis) (error 'sham/sam "fold-with-pattern/post-multiple: could not consume sequence ~a" is))
          (stack (cdr is) (cons `(multiple ,nos ,pat) os))])]
      [(ast:pat:repeat p k)
       (printf "fr: ~a\n" is) (pretty-print (pretty-path path))
       (match-define `(at-repeat ,frec ,path^) path)
       (define (enough? top rst nis)
         ;; TODO
         (printf "fr-en?: ~a ~a ~a\n" top rst nis)
         (or (empty? nis)
             (match top
               [`(ooo ,so ,k ,po) (false? k)]
               [else #f])))
       (let rec ([s (stack is `())]
                 [nk k])
         (match-define (stack nis nos) (frec s nk))
         (match nos
           [`(,top ,rst ...)
            (if (enough? top rst nis)
                (stack nis (cons `(repeat ,(cons top rst) ,pat) os))
                (rec (stack nis nos) nk))]))]))
  (car (stack-output (pattern-with-zipper f (stack (list stx) '()) pat))))

(define (rec-pattern p
                     for-single
                     for-datum
                     for-multiple
                     for-repeat)
  (define (rec pat)
    (match pat
      [(ast:pat:single c i) (for-single i pat)]
      [(ast:pat:datum s) (for-datum s pat)]
      [(ast:pat:multiple s) (for-multiple (vector-map rec s) pat)]
      [(ast:pat:repeat s k) (for-repeat (rec s) pat)]))
  (rec p))

;; returns a path to a specific field in the pattern o/w #f
;;   essentially a zipper which focusses on single pattern
;;   of the given `arg`
;; p@(pat:multiple ss) -> `(multiple s i p)
;; p@(pat:repeat s) -> `(repeat s p)
(define (find-arg pat arg-sym (=? equal?))
  (let/cc k
    (define (f _ pat path)
      (match pat
        [(ast:pat:single chk i) (if (=? i arg-sym) (k (cons pat path)) #f)]
        [(ast:pat:repeat p k) (match path [`(at-repeat ,frec ,pp) (frec #f k)])]
        [(ast:pat:multiple ps) (match path [`(at-multiple ,frec ,pp) (for ([i (vector-length ps)]) (frec #f i)) #f])]
        [else #f]))
    (pattern-with-zipper f #f pat)))

(module+ test
  (require rackunit)
  (provide (all-defined-out))
  (match-define (list a b c d e f) (syntax->list (datum->syntax #f `(a b c d e f))))
  (define (mlt . ps) (ast:pat:multiple (list->vector ps)))
  (define (sng sym) (ast:pat:single #f sym))
  (define (dat v) (ast:pat:datum v))
  (define (rpt p (k #f)) (ast:pat:repeat p k))

  (define p1 (sng a))
  (define p2 (dat 'a))
  (define p3 (mlt p2 p1 (sng b) (sng c)))
  (define p4 (mlt (rpt p3) (sng a) (dat 'a)))
  (check-match (find-arg p3 a) `(,_ . (in-multiple 1 ,_ ,_)))
  (check-false (find-arg p3 f))

  (define (p-v v p) (pretty-print p) (newline) p)
  #;(pattern-with-zipper p-v p-v p-v
                         (lambda (v f p) (pretty-print p) p)
                         #f p4)

  (define (datum=? v d) (equal? (syntax->datum v) d))
  (define s1 (datum->syntax #f `(1 2 3)))
  (check-match (parse-with-pattern (sng a) #`42)
               `(single ,s ,p))

  (check-match (parse-with-pattern (mlt (sng a) (sng b)) #`(a b))
               `(multiple ((single ,sb ,pb) (single ,sa ,pa)) ,pm)
               (and (equal? pa (sng a))
                    (equal? pb (sng b))
                    (equal? pm (mlt (sng a) (sng b)))))
  (check-match (parse-with-pattern (mlt (sng a) (mlt (sng b) (sng c))) #`(a (b c)))
               `(multiple ((multiple ((single ,sb ,pb) (single ,sc ,pc)) ,smm) (single ,sa ,pa)) ,sm))

  (define rs1 (mlt (rpt (sng a))))
  (check-match (parse-with-pattern rs1 #`(a))
               `(multiple ((repeat ((single ,s ,p)) ,rp)) ,mp)
               (and (datum=? s 'a)))
  (check-match (parse-with-pattern rs1 #`(a0 a1))
               `(multiple ((repeat ((single ,s1 ,p1) (single ,s0 ,p0)) ,rp)) ,mp)
               (and (datum=? s0 'a0) (datum=? s1 'a1) (equal? p0 (sng a)) (equal? p1 (sng a))))
  (check-match (parse-with-pattern rs1 #`(a0 a1 a2))
               `(multiple ((repeat ((single ,s2 ,p2) (single ,s1 ,p1) (single ,s0 ,p0)) ,rp)) ,mp)
               (and (datum=? s0 'a0) (datum=? s1 'a1) (datum=? s2 'a2)))
  (check-match (parse-with-pattern rs1 #'(a* (... ...)))
               `(multiple ((repeat ((ooo ,os #f ,op) (single ,a* ,a*p)) ,rp)) ,mp)
               (and (datum=? a* 'a*) (equal? a*p (sng a))))
  (check-match (parse-with-pattern rs1 #`(a0 a* (... ...)))
               `(multiple ((repeat ((ooo ,os #f ,op) (single ,a* ,a*p) (single ,s0 ,p0)) ,rp)) ,mp)
               (and (datum=? a* 'a*) (equal? a*p (sng a))))

  (define rs2 (mlt (rpt (sng a)) (sng b)))
  (check-match (parse-with-pattern rs2 #'(a* (... ...) b))
               `(multiple ((single ,sb ,pb) (repeat ((ooo ,os ,f ,op) (single ,a* ,a*p)) ,rp)) ,mp)
               (and (datum=? a* 'a*) (equal? a*p (sng a)) (datum=? sb 'b)))
  ;; (check-match (parse-with-pattern rs2 #'(a0 a1 b))
  ;;              `(multiple ((repeat ((single ,a* ,a*p) (ooo ,os ,f ,op)) ,rp) (single ,sb ,pb)) ,mp)
  ;;              (and (datum=? a* 'a*) (equal? a*p (sng a)) (datum=? sb 'b)))

  (parse-with-pattern (mlt (sng a) (sng b) (sng c)) s1)
  (parse-with-pattern p3 s1)

  (define plam (mlt (dat 'lam) (mlt (rpt (mlt (sng a) (sng b)))) (sng c)))
  (parse-with-pattern plam (datum->syntax #f `(([a 1] [b 2] [c 3]) d)))
  (parse-with-pattern plam (datum->syntax #f `(([i v] ...) d)))

  (define pneg (mlt (dat '-) (sng a) (rpt (sng b))))
  (parse-with-pattern pneg (datum->syntax #f `(a b)))

  (define pneg2 (mlt (dat '-) (sng a) (rpt (sng b)) (sng c)))
  ;; (parse-with-pattern pneg2 (datum->syntax #f `(a b c)))

  (define pft (mlt (rpt (sng a)) (sng b) (sng c)))
  (parse-with-pattern pft (datum->syntax #f '(a ... b c)))
  )
