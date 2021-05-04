#lang racket

(require (submod "private/spec.rkt" type)
         "private/utils.rkt")

(provide (all-defined-out))

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

(define (intrinsic-type? t) (member t '(string integer int bool boolean symbol)))
(define (id&type raw-id (c #f) (depth #f) (maybe-type #f))
  (define (from-specified k typs)
    (match* (k typs)
      [(k (list g ns ...))
       (match (syntax->datum g)
         [(or 'id 'identifier) (ast:type:identifier depth)]
         [(? intrinsic-type? t) (ast:type:intrinsic depth t)]
         [else (ast:type:internal depth typs)])]))
  (cond
    [c (values raw-id (ast:type:check c depth))]
    [else
     (match (split-identifier raw-id)
       [`(! ,intr) (values intr (ast:type:intrinsic intr depth))]
       [`(#f ,s) (values s (if maybe-type maybe-type (ast:type:unknown depth)))] ;; check type for specific id
       [`(,k ,id ,typs ...) (values id (from-specified k typs))]
       [else (values raw-id (ast:type:unknown depth))])]))

(define (node-arg-id&type c s depth info)
  (id&type s c depth (info-value 'type info)))
(define (group-arg-id&type id ainfo ginfo)
  (id&type id #f #f (or (info-value 'type ainfo) (info-value 'common-type ginfo))))

(module+ test
  (require rackunit)
  (check-equal? (split-id-string "!integer") (cons '! (list "integer")))
  (check-equal? (split-id-string "a:b.c") (cons ': (list "a" "b" "c")))
  (check-equal? (split-id-string "b_a") (cons '_ (list "a" "b")))
  (check-equal? (split-id-string "b.c_a") (cons '_ (list "a" "b" "c")))
  (check-equal? (split-id-string "a") (cons #f (list "a"))))
