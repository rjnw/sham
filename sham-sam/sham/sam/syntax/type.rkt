#lang racket

(require "private/spec.rkt"
         "private/utils.rkt")

(provide (all-defined-out))

(struct ast:type () #:prefab)
(struct ast:type:metadata ast:type () #:prefab)
(struct ast:type:internal ast:type (of depth) #:prefab)
(struct ast:type:external ast:type (of depth) #:prefab)
(struct ast:type:identifier ast:type () #:prefab)
(struct ast:type:unboxed ast:type (of) #:prefab)

;;; splits strings following type notation for ast specification
(define (split-id-string str)
  (match str
    [(regexp #rx"^!(.*)$" (list _ checker)) (cons '! (list checker))]
    [(regexp #rx"^(.*):(.*)$" (list _ id typ)) (cons ': (list id typ))]
    [(regexp #rx"^(.*)_(.*)$" (list _ typ id)) (cons '_ (list id typ))]
    [else (cons #f (list str))]))

(define (split-identifier syn)
  (define back-to-syn (curryr string->syntax syn))
  (match (split-id-string (syntax->string syn))
    [(cons kind ls) (cons kind (map back-to-syn ls))]))

(define id-without-type (compose cadr split-identifier))
(define type-from-id (compose cddr split-identifier))

(define (build-node-arg-type c s depth info)
  (void)
  ;; (if c
  ;;     (ast:type:external c depth)
  ;;     (match (type-from-id s)
  ;;       [`("!") (ast:type:external (id-without-type s) depth)]
  ;;       [t (ast:type:internal t depth)]))
  )
(define (build-group-arg-type id ginfo) (void))
(define (node-arg-info syn typ ninfo) '())
;; (define (group-arg-type-assoc gargs spec)
;;   (match-define (ast:group id parent args nodes ginfo) spec)
;;   (for/list ([orig (info-value ginfo `#:common)]
;;              [fmted gargs])
;;     (cons orig
;;           (match (symbol->string (syntax->datum fmted))
;;             [(regexp #rx"^!identifier$") (ast:type:identifier)]
;;             [(regexp #rx"^!(.*)$" (list _ checker)) (ast:type:unboxed checker)]
;;             [(pregexp #px"^(.*):(.*)" (list _ id type))
;;              (ast:type:internal
;;               (map (compose (curry datum->syntax #f) string->symbol)
;;                    (string-split type ".")) 0)]))))

;; (define (node-arg-type nspec)
;;   (match-define (ast:node id args pat info) nspec)
;;   (define (rec pat depth)
;;     (match pat
;;       [(ast:pat:single c s) (ast:node:arg s (if c (ast:type:external c depth) (type-from-id s depth)) #f)]
;;       [(ast:pat:datum d) #f]
;;       [(ast:pat:multiple s) (map (curryr rec depth) (vector->list s))]
;;       [(ast:pat:repeat r k) (rec r (add1 depth))]))
;;   (filter (compose not false?) (flatten (rec pat 0))))

(module+ test
  (require rackunit)
  (check-equal? (split-id-string "!integer") (cons '! (list "integer")))
  (check-equal? (split-id-string "a:b.c") (cons ': (list "a" "b.c")))
  (check-equal? (split-id-string "b_a") (cons '_ (list "a" "b")))
  (check-equal? (split-id-string "b.c_a") (cons '_ (list "a" "b.c")))
  (check-equal? (split-id-string "a") (cons #f (list "a"))))
