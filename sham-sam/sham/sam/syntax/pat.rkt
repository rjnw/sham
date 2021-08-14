#lang racket

(require "utils.rkt"
         "ooo.rkt")

(provide (all-defined-out))

(struct pat [])

(struct pat:var pat [s])
(struct pat:dat pat [v])
(struct pat:seq pat [ps])
(struct pat:alt pat [ps])
(struct pat:ooo pat [p k])
(struct pat:app pat [o r])

(define pat/c
  (flat-rec-contract pat/c
   (struct/c pat:var syntax?)
   (struct/c pat:dat any/c)
   (struct/c pat:seq (vector-immutableof pat/c))
   (struct/c pat:alt (listof pat/c))
   (struct/c pat:ooo pat/c ooo-count/c)
   (struct/c pat:app any/c (listof pat/c))))

(define (pat-path/c val/c)
  (flat-rec-contract pat-path/c
   (list/c)
   (list/c 'at-seq (-> val/c natural-number/c val/c) pat-path/c)
   (list/c 'in-seq pat? natural-number/c pat-path/c)
   (list/c 'at-alt (-> val/c natural-number/c val/c) pat-path/c)
   (list/c 'in-alt pat? natural-number/c pat-path/c)
   (list/c 'at-ooo (-> val/c (or/c false/c natural-number/c) val/c))
   (list/c 'in-ooo pat? (or/c false/c natural-number/c) pat-path/c)))

(define (pretty-pat p)
  (match p
    [(pat:var v) `(~v ,(syntax-e v))]
    [(pat:dat v) v]
    [(pat:seq ps) (for/list ([p ps]) (pretty-pat p))]
    [(pat:alt ps) (for/list ([p ps]) (pretty-pat p))]
    [(pat:ooo op k) `(~r ,(pretty-pat op) ,k)]
    [(pat:app op rands) `(,op ,@(map pretty-pat rands))]))

(define (pretty-path path)
  (match path
    [`() '()]
    [`(at-seq ,frec ,ppath) `(at-seq ,(pretty-path ppath))]
    [`(in-seq ,pat ,i ,ppath) `(in-seq ,(pretty-pat pat) ,i ,(pretty-path ppath))]
    [`(at-alt ,frec ,ppath) `(at-alt ,(pretty-path ppath))]
    [`(in-alt ,pat ,i ,ppath) `(in-alt ,(pretty-pat pat) ,i ,(pretty-path ppath))]
    [`(at-ooo ,frec ,ppath) `(at-ooo ,(pretty-path ppath))]
    [`(in-ooo ,pat ,k ,ppath) `(in-ooo ,(pretty-pat pat) ,k ,(pretty-path ppath))]))

;; f := val/c ast:pat/c pattern-path/c -> val/c
;;  folds over pattern along with a zipper containing the position of current pattern
;;  f is also polymorphic for pre and post multiple values
;;    repeat pattern is done with a special path containing a recursive function to perform
;;    the fold differently
(define (pat-zipper f val pat)
  (define (go path val pat)
    (match pat
      [(pat:var s) (f val pat path)]
      [(pat:dat v) (f val pat path)]
      [(pat:alt ps)
       (define (frec cval i)
         (go `(in-alt ,pat ,i ,path) cval (list-ref ps i)))
       (f val pat `(at-alt ,frec ,path))]
      [(pat:seq ps)
       (define (frec cval i)
         (go `(in-seq ,pat ,i ,path) cval (vector-ref ps i)))
       (f val pat `(at-seq ,frec ,path))]
      [(pat:ooo p k)
       (define (frec cval nk)
         (go `(in-ooo ,pat ,nk ,path) cval p))
       (f val pat `(at-ooo ,frec ,path))]))
  (go null val pat))


(struct stk [inp out] #:prefab)
(struct err [inf] #:prefab)

(define (dat-equal? dat stx)
 (and (identifier? stx) (equal? (syntax->datum stx) dat)))

(define (f-parse-stx-with-pattern val pat path (dat=? dat-equal?))
  (match val
    [(err inf) (err `(in ,inf ,pat))]
    [(stk (cons crnt rst) os)
     #:when (ooo? crnt)
     (stk rst (cons `(ooo ,crnt ,(ooo crnt) ,pat) os))]

    [(stk is os)
     #:when (pat:ooo? pat)
     (match-define (pat:ooo p (cons mn mx)) pat)
     ;; (printf "fr: ~a\n" is) (pretty-print (pretty-path path))
     (match-define `(at-ooo ,frec ,ppath) path)
     ;;; min-req-after: minimum value of syntax elements required after the current repeat
     ;;   this is non-zero only if the previous pattern is a multiple
     ;;   and, there are patterns after current repeat requiring some minimum elements
     (define min-req-after
       (match ppath
         [`(in-seq ,(pat:seq ps) ,idx ,_)
          (for/fold ([n 0])
                    ([p (vector-drop ps (add1 idx))])
            (match p
              [(pat:ooo p (cons mn mx))
               (if mn (+ mn n) n)]
              [else (add1 n)]))]
         [else 0]))
     ;; (printf "fr-min-req-after: ~a\n" min-req-after)
     (define (enough? top rst nis)
       ;; (printf "fr-en?: \ntop\t~a \nrst\t~a \nnis\t~a\n" top rst nis)
       (define (consumed-min? lst need)
         (if need
             (match lst
               [`() (zero? need)]
               [(cons `(ooo ,is (,omn . ,omx) ,pat) rst)
                (consumed-min? rst (if (false? omx) omx (- need omx)))]
               [else (consumed-min? (cdr lst) (sub1 need))])
             #t))
       (define (consumed-max? lst cnt) ;; cnt #f = never-enough
         (if cnt
             (match lst
               [`() (zero? cnt)]
               [(cons `(ooo ,is (,omn . ,omx) ,pat) rst)
                (consumed-max? rst (if omx (- cnt omx) cnt))]
               [else (consumed-max? (cdr lst) (sub1 cnt))])
             #f))
       (and (consumed-min? (cons top rst) mn)
            (or (consumed-max? (cons top rst) mx)
                (<= (length nis) min-req-after))))
     (if (list? is)
         (if (equal? min-req-after (length is))
             (stk is (cons `(repeat () ,pat) os))
             (let rec ([cval (stk is `())]
                       [nk (cons mn mx)])
               (cond [(err? cval) (err `(ooo "in-ooo" ,pat ,nk ,cval))]
                     [(stk? cval)
                      (match-define (stk nis nos) (frec cval nk))
                      (match nos
                        [`(,top ,rst ...)
                         (if (enough? top rst nis)
                             (stk nis (cons `(ooo ,(cons top rst) ,pat) os))
                             (rec (stk nis nos) nk))]
                        [else (err `(ooo "not-enough-ooo" ,pat ,nos ,nk))])])))
         (err `(ooo "no-stx-list" ,pat ,is)))]

    [(stk (cons fst rst) os)
     ;; (printf "fpwp: \nis = ~a, os = ~a\n" is os) (pretty-print (pretty-path path))
     (match pat
       [(pat:var id)
        (stk rst (cons `(var ,fst ,pat) os))]

       [(pat:dat val)
        (cond [(dat=? val fst) (stk rst (cons `(dat ,fst ,pat) os))]
              [else (err `(dat "datum-not-equal" ,pat ,val ,fst))])]

       [(pat:seq ps)
        #:when (not (list? (syntax-e fst)))
        (err `(seq "no-stx-list" ,pat ,fst))]
       [(pat:seq ps)
        (match-define `(at-seq ,frec ,ppath) path)
        (define nval
          (let rec ([cval (stk (syntax-e fst) '())]
                    [i 0])
            ;;; rec goes through the input list with index over length of pms
            (cond [(err? cval) (err `(seq "in-seq" ,pat ,i ,cval))]
                  [(equal? i (vector-length ps)) cval]
                  [else (rec (frec cval i) (add1 i))])))
        (match nval
          [(stk '() nos) (stk rst (cons `(seq ,nos ,pat) os))]
          [(stk nis nos) (err `(seq "leftover-seq" ,pat ,nis ,nos ,os))]
          [else nval])]

       [(pat:alt ps)
        (match-define `(at-alt ,frec ,ppath) path)
        (let rec ([cval (stk (list fst) '())]
                  [i 0])
          ;; (printf "pat:alt: ~a ~a\n" cval i)
          (cond [(and (stk? cval) (empty? (stk-inp cval)))
                 (match-define (stk '() nos) cval)
                 (stk rst (cons `(alt ,nos ,(sub1 i) ,pat) os))]
                [(equal? i (length ps)) (err `(alt "no-alt-match" ,pat))]
                [(err? cval) (rec (frec (stk (list fst) '()) i) (add1 i))]
                [(zero? i) (rec (frec cval i) (add1 i))]
                [else (error 'sham/sam "internal: pat:alt in parse-with-zipper ~a ~a" cval i)]))])]
    [else (err `(any "no syntax for pattern" ,pat ,path))]))

(define (parse-stx-with-pattern pat stx)
  ;;; uses pattern-with-zipper function to recurse through the pattern
  ;;   our state has two parts
  ;;   input: current input list of syntax
  ;;   output tagged result containing parsed syntax
  ;;   output contains syntax in reverse order as a stack
  (pat-zipper f-parse-stx-with-pattern (stk (list stx) '()) pat))

;; parses the syntax according to a given pattern and returns tagged syntax with corresponding patterns
;;  the sequences in result are in reverse order
(define (stx-path-for-pattern pat stx)
  (match (parse-stx-with-pattern pat stx)
    [(stk '() (list out)) out]
    [(stk inp out) (error 'sham:sam "leftover syntax after parsing ~a ~a" inp out)]
    [(err inf) (error 'sham:sam "error while parsing ~a" inf)]))

;; returns a path to a specific pattern o/w #f
;;   essentially a zipper which focusses on single pattern
;;   of the given `arg`
;; p@(pat:multiple ss) -> `(multiple s i p)
;; p@(pat:repeat s) -> `(repeat s p)
#;(define (find-pattern pat f?)
  (let/cc k
    (define (f _ pat path)
      (match pat
        [p #:when (f? pat) (k (cons pat path))]
        [(pat:ooo p k) (match path [`(at-ooo ,frec ,pp) (frec #f k)])]
        [(pat:seq ps) (match path [`(at-seq ,frec ,pp) (for ([i (vector-length ps)]) (frec #f i)) #f])]
        [else #f]))
    (pat-zipper f #f pat)))

(module+ test
  (require rackunit)
  (provide (all-defined-out))
  (match-define (list a b c d e f) (syntax->list (datum->syntax #f `(a b c d e f))))
  (define (var v) (pat:var v))
  (define (dat v) (pat:dat v))
  (define (seq . ps) (pat:seq (apply vector-immutable ps)))
  (define (alt . ps) (pat:alt ps))
  (define (rpt p (mn #f) (mx #f)) (pat:ooo p (cons mn mx)))
  (define (app op . ps) (pat:app op ps))

  (define-values (pa pb pc pd pe pf) (values (var a) (var b) (var c) (var d) (var e) (var f)))

  (define p1 pa)
  (define p2 (dat 'λ))
  (define p3 (seq pa pb))
  (define p4 (seq p2 pa pb pc))
  (define p5 (alt p1 p2))
  (define p6 (seq (rpt p1)))
  (define p7 (seq (rpt p1) p2))
  (define p8 (seq (rpt p1) p2 (rpt p1)))
  (define p9 (seq (rpt (alt p2 p1))))

  (define psp parse-stx-with-pattern)

  (check-match (psp p1 #'42) (stk '() `((var ,s42 ,opa)))
               (equal? opa pa))
  (check-match (psp p2 #'λ) (stk '() `((dat ,sλ ,op2)))
               (equal? op2 p2))
  (check-match (psp p2 #'lambda) (err `(dat "datum-not-equal" ,op2 λ ,is)))

  (check-match (psp p3 #'21) (err `(seq . ,sinf)))
  (check-match (psp p3 #'(a b)) (stk '() `((seq ((var ,sb ,opb) (var ,sa ,opa)) ,ops))))

  (check-match (psp p4 #'(λ a b c))
               (stk '() `((seq ((var ,osc ,opc) (var ,osb ,opb) (var ,osa ,opa) (dat ,osλ ,opλ)) ,ops))))
  (check-match (psp p5 #'a) (stk '() `((alt ((var ,osa ,opa)) 0 ,op))))

  (check-match (psp (alt (dat 'a) (dat 'b)) #'0)
               (err `(alt "no-alt-match" . ,inf)))

  (check-match (psp p6 #'(a)) (stk '() `((seq ((ooo ((var ,osa ,opa)) ,opo)) ,op))))

  (check-match (psp p7 #`(a λ))
               (stk '() `((seq ((dat ,osλ ,opλ)
                                (ooo ((var ,osa ,opa)) ,opo))
                               ,op))))

  (check-match (psp p7 #`(a)) (err `(seq "in-seq" ,ps 2 ,(err `(dat "datum-not-equal" ,opd ,ose ,osi)))))

  (check-match (psp p8 #`(a b λ c d)) (err inf)) ;; maybe-TODO

  (check-match (psp p9 #`(a λ))
               (stk '() `((seq ((ooo ((alt ((dat ,osλ1 ,opλ1)) 0 ,opa1)
                                      (alt ((var ,osa ,opa)) 1 ,opa2))
                                     ,opo))
                               ,ops))))
  )
