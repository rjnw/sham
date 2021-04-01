#lang racket

(require (for-template racket))
(require "spec.rkt")
(provide fold-with-pattern
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
                     (list/c 'pre-multiple pattern-path/c)
                     (list/c 'multiple natural-number/c (listof ast:pat/c) pattern-path/c)
                     (list/c 'post-multiple pattern-path/c)
                     (list/c 'repeat ast:pat/c (or/c false/c natural-number/c) pattern-path/c)
                     (list/c 'at-repeat (-> val/c (or/c false/c natural-number/c) val/c))))

(define (pretty-path path)
  (match path
    [`() '()]
    [`(multiple ,i ,pats ,ppath) `(multiple ,i ,(map pretty-pattern (vector->list pats)) ,(pretty-path ppath))]
    [`(post-multiple ,ppath) `(>-multiple ,(pretty-path ppath))]
    [`(pre-multiple ,ppath) `(<-multiple ,(pretty-path ppath))]
    [`(repeat ,pat ,k ,ppath) `(repeat ,(pretty-pattern pat) ,k ,(pretty-path ppath))]
    [`(at-repeat ,frec ,ppath) `(@-repeat ,(pretty-path ppath))]))

;; f := val/c ast:pat/c pattern-path/c -> val/c
;;  folds over pattern along with a zipper containing the position of current pattern
;;  f is also polymorphic for pre and post multiple values
;;    repeat pattern is done with a special path containing a recursive function to perform
;;    the fold differently
(define (fold-with-zipper f val pat)
  (define (go path val pat)
    (match pat
      [(ast:pat:single chk id) (f val pat path)]
      [(ast:pat:datum d) (f val pat path)]
      [(ast:pat:multiple ps) (f (for/fold ([cval (f val pat `(pre-multiple ,path))])
                                          ([p (in-vector ps)]
                                           [i (vector-length ps)])
                                  (define npath `(multiple ,i ,ps ,path))
                                  (go npath cval p))
                                pat
                                `(post-multiple ,path))]
      [(ast:pat:repeat p k)
       (define (frec cval nk)
         (go `(repeat ,p ,nk ,path) cval p))
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
(define (fold-with-pattern pat stx
                          #:single (for-single default-single-format)
                          #:datum (for-datum default-datum-format)
                          #:multiple (for-multiple default-multiple-format)
                          #:repeat (for-repeat default-repeat-format))
  (struct sctxt [istx ostx klist] #:prefab)
  (define (f sc pat path)
    (match-define (sctxt is os kl) sc)
    (match pat
      [(ast:pat:single chk id)
       (printf "fs: ~a\n" (sctxt-istx sc)) (pretty-print (pretty-path path))
       (match path
         [`(multiple ,i ,ps (repeat ,p ,nk ,ppath))
          #:when (false? is) (sctxt is os kl)]
         [`(multiple ,i ,ps ,ppath)
          (sctxt (cdr is) (os (for-single (car is) pat)) kl)]
         [`(repeat ,p ,nk ,ppath)
          (match (ooo stx)
            [#f (sctxt (cdr is) (os (for-single (car is) pat)) (consume kl))]
            [(list n) (sctxt (cdr is) (os (car is)) (consume kl n))])])]
      [(ast:pat:datum val) (sctxt is os kl)];;TODO remove if datum needed before providing reader
      [(ast:pat:multiple pms)
       (printf "fm: ~a\n" (sctxt-istx sc)) (pretty-print (pretty-path path))
       (define ooo-n (cond [(syntax? is) (ooo is)]
                           [(cons? is) (ooo (car is))]
                           [else #f]))
       (match* (ooo-n path)
         [((list n) `(pre-multiple (repeat ,p ,nk ,ppath)))
          (printf "fm/... ~a\n" n)
          (define lis (cond [(cons? is) (cdr is)]
                            [else #f]))
          (define (gen-multf ostx)
            (lambda (stx)
              (cond [(syntax? stx) (gen-multf (cons stx ostx))]
                    [(false? stx) (list lis ostx os)]
                    [else (error 'sham/sam "fold-with-pattern/...")])))
          (sctxt #f (gen-multf (list (car is))) (consume kl n))]
         [('#f `(pre-multiple ,prev-path))
          (define-values (cis lis)
            (match prev-path
              [`(multiple ,_ ,_ ,_) (values (car is) (cdr is))]
              [`(repeat ,_ ,_ ,_) (values (car is) (cdr is))]
              [`() (values is #f)]))
          (printf "fm/pre-mult: ~a ~a ~a\n" cis lis (pretty-path prev-path))
          (define (gen-multf ostx)
            (lambda (stx)
              (cond [(syntax? stx) (gen-multf (cons stx ostx))]
                    [(false? stx) (list lis ostx os)]
                    [else (error 'sham/sam "fold-with-pattern/gen-multf: ~a" stx)])))
          (sctxt (syntax-e cis) (gen-multf '()) kl)]
         [(_ `(post-multiple ,ppath))
          #:when (false? is)
          (match-define (list lis ms prev) (os #f))
          (sctxt lis (prev (reverse ms)) kl)]
         [(_ `(post-multiple ,path))
          (unless (empty? is) (error 'sham/sam "fold-with-pattern/post-multiple: cannot consume sequence ~a" is))
          (match (os #f)
            [(list lis ms prev) (sctxt lis (prev (for-multiple (reverse ms) pat)) kl)])])]
      [(ast:pat:repeat p k)
       (printf "fr: ~a\n" (sctxt-istx sc)) (pretty-print (pretty-path path))
       (match-define `(at-repeat ,frec ,path^) path)
       (define (gen-repf ostx)
         (lambda (stx)
           (cond [(syntax? stx) (gen-repf (cons stx ostx))]
                 [(list? stx) (gen-repf (append stx ostx))]
                 [(false? stx) (list is ostx os)]
                 [else (error 'sham/sam "fold-with-pattern/gen-repf: ~a" stx)])))
       (let rec ([s (sctxt is (gen-repf '()) (cons k kl))] [nk k])
         (match-define (sctxt nis nos nkl) (frec s nk))
         (match-define (cons n kl^) nkl)
         (cond [(or (and (false? n) (empty? nis)) (equal? n 0))
                (match-define (list is rstxs prev-os) (nos #f))
                (sctxt nis (prev-os (for-repeat (reverse rstxs) pat)) kl)]
               [else (rec (sctxt nis nos nkl) n)]))]))
  (sctxt-ostx (fold-with-zipper f (sctxt stx identity null) pat)))

(define (map-with-pattern2 pat stx
                          #:single (for-single default-single-format)
                          #:datum (for-datum default-datum-format)
                          #:multiple (for-multiple default-multiple-format)
                          #:repeat (for-repeat default-repeat-format))

  ;; k : syntax? number? -> syntax?
  ;;   continuation with final match syntax and leftover repeat value
  (define ((do-head pat stx) k c)
    ;; (printf "do-head: \n\tpat: ~a, \n\tstx: ~a, \n\tc: ~a\n" (pretty-pattern pat) (syntax->datum stx) c)
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
;; p@(pat:multiple ss) -> `(multiple s i p)
;; p@(pat:repeat s) -> `(repeat s p)
(define (find-arg pat arg-sym (=? equal?))
  (let/cc k
    (define (f _ pat path)
      (match pat
        [(ast:pat:single chk i) (if (=? i arg-sym) (k (cons pat path)) #f)]
        [(ast:pat:repeat p k) (match path [`(at-repeat ,frec ,pp) (frec #f k)])]
        [else #f]))
    (fold-with-zipper f #f pat)))

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
  (check-match (find-arg p3 a) `(,_ . (multiple 1 ,_ ,_)))
  (check-false (find-arg p3 f))

  (define (p-v v p) (pretty-print p) (newline) p)
  #;(fold-with-zipper p-v p-v p-v
                      (lambda (v f p) (pretty-print p) p)
                      #f p4)

  (define s1 (datum->syntax #f `(1 2 3)))
  (printf "mwp:\n")
  (fold-with-pattern (mlt (sng a) (sng b) (sng c)) s1)
  (fold-with-pattern p3 s1)

  (define plam (mlt (dat 'lam) (mlt (rpt (mlt (sng a) (sng b)))) (sng c)))
  (fold-with-pattern plam (datum->syntax #f `(([a 1] [b 2] [c 3]) d)))
  (fold-with-pattern plam (datum->syntax #f `(([i v] ...) d)))

  (define pneg (mlt (dat '-) (sng a) (rpt (sng b))))
  (fold-with-pattern pneg (datum->syntax #f `(a b)))

  (define pneg2 (mlt (dat '-) (sng a) (rpt (sng b)) (sng c)))
  ;; (fold-with-pattern pneg2 (datum->syntax #f `(a b c)))
  )
