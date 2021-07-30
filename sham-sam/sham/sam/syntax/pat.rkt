#lang racket

(provide (all-defined-out))

(struct pat [])

(struct pat:syn pat [s])
(struct pat:dat pat [v])
(struct pat:seq pat [ps])
(struct pat:ooo pat [p k])            ;; k := (cons min max)
(struct pat:app pat [o r])

(define (pat-path/c val/c)
  (flat-rec-contract
   pat-path/c
   (list/c)
   (list/c 'in-seq pat? natural-number/c pat-path/c)
   (list/c 'at-seq (-> val/c natural-number/c val/c) pat-path/c)
   (list/c 'in-ooo pat? (or/c false/c natural-number/c) pat-path/c)
   (list/c 'at-ooo (-> val/c (or/c false/c natural-number/c) val/c))))

(define (pretty-pat pattern)
    (match pattern
      [(ast:pat:syn s) `(~s ,(syntax-e id))]
      [(ast:pat:datum v) `',v]
      [(ast:pat:multiple ps) (for/list ([p ps]) (pretty-pat p))]
      [(ast:pat:repeat spec k) `(~r ,(pretty-pat spec) ,k)]))

(define (pretty-path path)
  (match path
    [`() '()]
    [`(in-seq ,pat ,i ,ppath) `(in-seq ,(pretty-pat pat) ,i ,(pretty-path ppath))]
    [`(at-seq ,frec ,ppath) `(at-seq ,(pretty-path ppath))]
    [`(in-ooo ,pat ,k ,ppath) `(in-ooo ,(pretty-pat pat) ,k ,(pretty-path ppath))]
    [`(at-ooo ,frec ,ppath) `(at-ooo ,(pretty-path ppath))]))

;; f := val/c ast:pat/c pattern-path/c -> val/c
;;  folds over pattern along with a zipper containing the position of current pattern
;;  f is also polymorphic for pre and post multiple values
;;    repeat pattern is done with a special path containing a recursive function to perform
;;    the fold differently
(define (pat-zipper f val pat)
  (define (go path val pat)
    (match pat
      [(pat:syn s) (f val pat path)]
      [(pat:dat v) (f val pat path)]
      [(pat:seq ps)
       (define (frec cval i)
         (go `(in-seq ,pat ,i ,path) cval (vector-ref ps i)))
       (f val pat `(at-seq ,frec ,path))]
      [(pat:ooo p k)
       (define (frec cval nk)
         (go `(in-ooo ,pat ,nk ,path) cval p))
       (f val pat `(at-ooo ,frec ,path))]))
  (go null val pat))

;; parses the syntax according to a given pattern and returns tagged syntax with corresponding patterns
;;  the sequences in result are in reverse order
(define (parse-with-pattern opat stx)
  ;;; uses pattern-with-zipper function to recurse through the pattern
  ;;   our state has two parts
  ;;   input: current input list of syntax
  ;;   output tagged result containing parsed syntax
  ;;   output contains syntax in reverse order as a stack

  ;; (printf "fwp: ~a" stx) (newline) (pretty-print (pretty-pattern opat)) (newline)
  (struct stack [input output] #:prefab)
  (define (f sc pat path)
    (match-define (stack is os) sc)
    (match pat
      [(pat:syn id)
       ;; (printf "fs: ~a\n" is) (pretty-print (pretty-path path)) (printf "\tos:~a\n" os)
       (when (empty? is)
         (error 'sham/sam "couldn't parse pattern: ~a in ~a ~a with stack\n" (pretty-pat pat) (pretty-pat opat) stx os))
       (match (ooo (car is))
         [(cons mn mx) (stack (cdr is) (cons `(ooo ,(car is) (,mn . ,mx) ,pat) os))]
         [else (stack (cdr is) (cons `(syn ,(car is) ,pat) os))])]
      [(pat:dat val)
       (cond [(and (identifier? (car is))
                   (equal? (syntax->datum (car is)) val))
              (stack (cdr is) (cons `(dat ,(car is) ,pat) os))]
             [else (stack is os)])]
      [(pat:seq ps)
       ;; (printf "fm: ~a\n" is) (pretty-print (pretty-path path))
       (match-define `(at-seq ,frec ,ppath) path)
       (match (ooo (car is))
         [(cons mn mx)
          (stack (cdr is) (cons `(ooo ,(car is) (,mn . ,mx) ,pat) os))]
         [#f
          ;; (printf "fm: ~a ~a ~a\n" is os (pretty-path prev-path))
          (match-define (stack nis nos)
            (let rec ([cval (stack (syntax-e (car is)) '())]
                      [i 0])
              ;;; rec goes through the input list with index over length of pms
              (cond [(equal? i (vector-length ps)) cval]
                    [else (rec (frec cval i) (add1 i))])))
          (unless (empty? nis)
            (error 'sham/sam
                   "fold-with-pattern/post-multiple: could not consume sequence ~a ~a" is stx))
          (stack (cdr is) (cons `(multiple ,nos ,pat) os))])]
      [(pat:ooo p (cons mn mx))
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
       (if (equal? min-req-after (length is))
           (stack is (cons `(repeat () ,pat) os))
           (let rec ([s (stack is `())]
                     [nk (cons mn mx)])
             (match-define (stack nis nos) (frec s nk))
             (match nos
               [`(,top ,rst ...)
                (if (enough? top rst nis)
                    (stack nis (cons `(ooo ,(cons top rst) ,pat) os))
                    (rec (stack nis nos) nk))])))]))
  (match-define (stack fin fou) (stack-output (pat-zipper f (stack (list stx) '()) opat)))
  (unless (and (empty? fin) (equal? (length fou) 1)) (error 'sham:sam "couldn't parse ~a correctly with ~a" stx opat))
  (car fou))

;; returns a path to a specific pattern o/w #f
;;   essentially a zipper which focusses on single pattern
;;   of the given `arg`
;; p@(pat:multiple ss) -> `(multiple s i p)
;; p@(pat:repeat s) -> `(repeat s p)
(define (find-pattern pat f?)
  (let/cc k
    (define (f _ pat path)
      (match pat
        [p #:when (f? pat) (k (cons pat path))]
        [(pat:ooo p k) (match path [`(at-ooo ,frec ,pp) (frec #f k)])]
        [(pat:seq ps) (match path [`(at-seq ,frec ,pp) (for ([i (vector-length ps)]) (frec #f i)) #f])]
        [else #f]))
    (pat-zipper f #f pat)))
