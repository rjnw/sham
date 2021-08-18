#lang racket

(require racket/syntax)
(provide (all-defined-out))

(define (find-first lst f?)
  (if (empty? lst)
      #f
      (or (and (f? (car lst)) (car lst))
          (find-first (cdr lst) f?))))

;; counts `c?` items in a sequence until `stop?`
;;  both c? and stop? take value and index
(define (count-until seq c? stop?)
  (for/fold ([c 0])
            ([v seq]
             [i (sequence-fold (λ (i v) (+ i 1)) 0 seq)]
             #:break (stop? v i))
    (+ c (if (c? v i) 1 0))))

;; contracts
(define (maybe/c x/c) (or/c false/c x/c))
(define (assoc/c key/c value/c) (listof (cons/c key/c value/c)))

;; syntax utils
(define syntax->string (compose symbol->string syntax->datum))
(define (string->syntax str (ctxt #f)) (datum->syntax ctxt (string->symbol str)))

(define (->symbol s)
  (cond [(syntax? s) (syntax->datum s)]
        [(string? s) (string->symbol s)]
        [(or (integer? s) (symbol? s)) s]
        [else (error 'sham/sam "->symbol: couldn't force ~a to symbol" s)]))
(define (punctuate ids sep)
  (for/fold ([i (if (>= (length ids) 1) (car ids) #'||)])
            ([id (if (cons? ids) (cdr ids) '())])
    (format-id #f "~a~a~a" i sep id)))

;;; splits strings following type notation for ast specification
(define (split-id-string str)
  (match str
    [(regexp #rx"^!(.*)$" (list _ checker)) (cons '! (list checker))]
    [(regexp #rx"^(.*):(.*)$" (list _ id typ)) (cons ': (cons id (string-split typ ".")))]
    [(regexp #rx"^(.*)_(.*)$" (list _ typ id)) (cons '_ (cons id (string-split typ ".")))]
    [else (cons #f (list str))]))

(define (split-identifier syn)
  (define back-to-syn (curryr string->syntax syn))
  (match (split-id-string (syntax->string syn))
    [(cons kind ls) (cons kind (map back-to-syn ls))]))

(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

(module+ test
  (require rackunit)
  (check-equal? (->symbol (punctuate `(a b c d) ":")) 'a:b:c:d)
  (check-equal? (split-id-string "!integer") (cons '! (list "integer")))
  (check-equal? (split-id-string "a:b.c") (cons ': (list "a" "b" "c")))
  (check-equal? (split-id-string "b_a") (cons '_ (list "a" "b")))
  (check-equal? (split-id-string "b.c_a") (cons '_ (list "a" "b" "c")))
  (check-equal? (split-id-string "a") (cons #f (list "a"))))
