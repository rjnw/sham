#lang racket

(require (for-template racket))
(require "spec.rkt")
(provide map-with-pattern
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
  (printf "consuming: ~a-~a\n" c n)
  (match c
    [(cons h t) (cons (if h (- h n) h) t)]))
(define (is-consumed? c)
  (match c
    [(list h t) (and (is-consumed? h) (is-consumed? t))]
    [null #t]
    [#f #t]
    [0 #t]
    [else #f]))

(define pattern-path/c
  (flat-rec-contract pattern-path/c
                     (list/c 'single any/c pattern-path/c)
                     (list/c 'multiple natural-number/c (listof any/c) pattern-path/c)
                     (list/c 'repeat (or/c false/c natural-number/c) pattern-path/c)
                     (list/c)))

;; path :=     ;; inside out path to the top pattern, haskell zippers
;;   `(single ,check ,identifier ,path)
;;   `(datum ,value ,path)
;;   `(multiple ,index ,(list pattern) ,path)
;;   `(repeat ,pattern ,k ,path)
(define (fold-with-zipper fs fd fm fr val pat)
  (define (go path val pat)
    (match pat
      [(ast:pat:single chk id) (fs val `(single ,chk ,id ,path))]
      [(ast:pat:datum d) (fd val `(datum ,d ,path))]
      [(ast:pat:multiple ps) (for/fold ([cval val])
                                       ([p (in-vector ps)]
                                        [i (vector-length ps)])
                               (define npath `(multiple ,i ,ps ,path))
                               (fm (go npath cval p) npath))]
      [(ast:pat:repeat p n)
       (let frec ([cval val]
                  [nv n])
         (define npath `(repeat ,p ,nv ,path))
         (fr (go npath cval p) frec npath))]))
  (go null val pat))

(define default-single-format identity)
(define default-datum-format (const #f))
(define (default-multiple-format ss) #`(vector . #,ss))
(define (default-repeat-format ss) #`(list . #,ss))

(define (map-with-pattern pat stx
                          #:single (for-single default-single-format)
                          #:datum (for-datum default-datum-format)
                          #:multiple (for-multiple default-multiple-format)
                          #:repeat (for-repeat default-repeat-format))
  ;; k : syntax? number? -> syntax?
  ;;   continuation with final match syntax and leftover repeat value
  (define ((do-head pat stx) k c)
    (printf "do-head: \n\tpat: ~a, \n\tstx: ~a, \n\tc: ~a\n" (pretty-pattern pat) (syntax->datum stx) c)
    (match (ooo stx)
      [(list n) (k pat (consume (list n)))]
      [#f
       (match pat
         [(ast:pat:single chk id) (k (for-single stx) (consume c))]
         [(ast:pat:datum v) (k (for-datum stx) c)]
         [(ast:pat:multiple pms)
          (match* ((remove-datum pms) (syntax-e stx))
            [((cons psh pst) (cons sth stt))
             ((do-head psh sth)
              (λ (s n) (k (for-multiple (filter (not/c false?) (cons s ((do-tail pst stt) psh n))))
                          (consume c)))
              `(1))])]
         [(ast:pat:repeat pr nr)
          (match (syntax-e stx)
            [(cons sth stt)
             (k ((do-head pr sth)
                 (λ (s n) (for-repeat (cons s
                                            ((do-tail #f stt) pr n))))
                 nr)
                (consume c))])])]))

  ;; pat : (or/c false? ast:pat?)
  ;;;  #f for pat signifies tail for a repeat pattern and to match against prev instead
  (define ((do-tail pat stx) prev c)
    (printf "do-tail: \n\tpat: ~a, \n\tstx: ~a, \n\tc: ~a\n"
            (if pat (map pretty-pattern pat) pat)
            (map syntax->datum stx) c)
    (match* (pat stx)
      [(null null) null]
      [((cons psh pst) (cons sth stt))
       (match (ooo sth)
         [(list n) (error "sham-sag: repeating match pattern for a non-repeating ast pattern")]
         [#f ((do-head psh sth)
              (λ (s n) (cons s ((do-tail pst stt) psh n)))
              c)])]
      [(#f null)
       (unless (is-consumed? c) (error "sham-sag: repeating pattern did not consume required number of terms"))
       null]
      [(#f (cons sth stt))
       (match (ooo sth)
         [(list n) ((do-tail pat stt) prev (consume c n))]
         [#f ((do-head prev sth)
              (λ (s n) (cons s ((do-tail pat stt) prev n)))
              c)])]))
  ((do-head pat stx) kid `(1)))

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
;; p@(pat:single i c) -> `(single i p)
;; p@(pat:multiple ss) -> `(multiple s i p)
;; p@(pat:repeat s) -> `(repeat s p)
(define (find-arg pat arg-sym (=? equal?))
  (let/cc k
    (define (fsingle _ path)
      (match-define `(single ,chk ,i ,p) path)
      (if (=? i arg-sym) (k path) #f))
    (define (fdatum d p) #f)
    (define (fmultiple v path) #f)
    (define (frepeat v f path) #f)
    (fold-with-zipper fsingle fdatum fmultiple frepeat #f pat)))

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
  (check-match (find-arg p3 a) `(single ,_ ,_ (multiple ,_ 1 ,_ ,_)))
  (check-false (find-arg p3 a))

  (define (p-v v p) (pretty-print p) (newline) p)
  (fold-with-zipper p-v p-v p-v
                    (lambda (v f p) (pretty-print p) p)
                    #f p4))
