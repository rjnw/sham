#lang racket

(require (for-template racket))
(require (submod "spec.rkt" ast)
         "pat.rkt"
         "utils.rkt")
(provide group-arg-storage
         node-args-storage
         from-node-storage)

(define (group-arg-storage gargs)
  #`(vector #,@gargs))

(define (node-args-storage nargs pat)
  ;; (printf "node-args-storage ~a\n\t ~a\n" (map pretty-arg nargs) (pretty-pattern pat))
  (match-define (cons f c)
    (let rec ([p pat])
      (match p
        [(pat:var stxid) (cons identity 1)]
        [(pat:seq ps)
         (define ps-results (vector->list (vector-map rec (vector-filter-not pat:dat? ps))))
         (define total-cnt (apply + (map cdr ps-results)))
         (define (thisf . all-args)
           #`(vector
              #,@(let rec ([args all-args]
                           [pr ps-results])
                   (match pr
                     ['() '()]
                     [(cons (cons f cnt) rst)
                      (cons (apply f (take args cnt))
                            (rec (drop args cnt) rst))]))))
         (cons thisf total-cnt)]
        [(pat:ooo p k)
         (match-define (cons pf pc) (rec p))
         (define pargs (generate-temporaries (build-list pc (const 'v))))
         (cons
          (λ args #`(map (λ #,pargs #,(apply pf pargs)) #,@args))
          pc)]
        [(pat:dat d) (cons (const #`#()) 0)])))
  (define (arg-id arg)
    (get-fid (ast:basic-id arg)))
  (apply f (map arg-id nargs)))

(define ((generate-access path) val)
  ;; (printf "generate-access: ") (pretty-print (pretty-path path))

  ;; writtin in cps as how to extract values of a certain node argument depends on both
  ;;  what is before in the path and also what is after. sequence only cares what is before in path
  ;;  and just vector-ref whereas ooo maps "how" to what comes after
  ;;  in essence k specifies how to get the value
  (let rec ([p path]
            [k identity])
    (match p
      [`() (k val)]
      [`(in-seq ,pat ,idx ,ppath)
       (define datum-until (count-until (pat:seq-ps pat) (λ (v i) (pat:dat? v)) (λ (v i) (equal? i idx))))
       (define without-datum-idx (- idx datum-until))
       (define vstxid (car (generate-temporaries '(v))))
       (rec ppath (λ (v) (k #`(vector-ref #,v #,without-datum-idx))))]
      [`(in-ooo ,p ,cnt ,ppath)
       (define lstxid (car (generate-temporaries '(l))))
       (rec ppath (λ (v) #`(map (λ (#,lstxid) #,(k lstxid)) #,v)))])))

;; checks whether given identifier matches with the given single pattern
(define (arg-pattern? id p)
  (match p
    [(ast:pat:single i c) (equal? i id)]
    [else #f]))

;; returns pattern path for the sub-pattern matching the given argument in full pattern
(define (arg-path arg pat)
  (cond
    [(find-pattern pat (curry arg-pattern? (get-oid (ast:basic-id arg)))) => cdr]
    [else #f]))

(define (from-node-storage arg pat)
  (cond [(arg-path arg pat) => generate-access]
        [else (error 'sham/sam/internal "could not find arg in pattern ~a ~a" (pretty-arg arg) (pretty-pattern pat))]))

(module+ test
  (require rackunit)
  (require (submod "pat.rkt" test))
  (define (node-arg i) (ast:node:arg `((0 . ,i) (f . ,i)) '() '()))
  (define aa (node-arg a))
  (define ba (node-arg b))
  (define ca (node-arg c))
  (define args (list aa ba ca))
  ;; (check-equal? (syntax->datum ((from-node-storage aa p1) f))
  ;;               `f)
  ;; (define pp (mlt (dat 'lambda) (mlt (rpt (mlt (sng a) (sng b))) (sng c))))
  ;; (check-equal? (syntax->datum ((from-node-storage aa pp) f))
  ;;               `(map (curryr vector-ref 0) (vector-ref (vector-ref f 0) 0)))
  ;; (check-equal? (syntax->datum ((from-node-storage ba pp) f))
  ;;               `(map (curryr vector-ref 1) (vector-ref (vector-ref f 0) 0)))
  ;; (define p2 (mlt (dat 'a) (sng a) (dat 'b) (dat 'c) (mlt (sng b) (sng c))))
  ;; (check-equal? (syntax->datum ((from-node-storage aa p2) f))
  ;;               `(vector-ref f 0))
  ;; (check-equal? (syntax->datum ((from-node-storage ba p2) f))
  ;;               `(vector-ref (vector-ref f 1) 0))

  ;; (check-equal? (syntax->datum (node-args-storage args plam))
  ;;               `(vector (vector (map vector a b)) c))
  (define drpt (seq (rpt (seq (rpt (seq pa pb))))))
  ;; (node-args-storage args drpt)
  (void)
  )
