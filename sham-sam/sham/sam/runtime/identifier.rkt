#lang racket
(require "ast.rkt")

(provide (all-defined-out))

(struct ast:scope [kind instance])
(define default-id-scope (ast:scope #f #f))
(define (new-id-scope kind) (ast:scope kind (gensym kind)))

(define ast:id/c (or/c symbol? syntax? ast:id?))
(define debug-id? (make-parameter #f))
(define (write-id v sym port mode)
  (define stxid (ast:id-stxid v))
  (define-values (dir fname rel?) (if (path? (syntax-source stxid)) (split-path (syntax-source stxid)) (values #f '- #f)))
  (if (debug-id?)
      (fprintf port
               "~a~a~a:~a:~a"
               (syntax->datum stxid)
               sym
               fname
               (syntax-line stxid)
               (syntax-column stxid))
      (fprintf port "~a~a" (syntax->datum stxid) sym)))

(struct ast:id:def ast:id [gen scope (maybe-refs #:mutable)]
 #:methods gen:custom-write
  [(define (write-proc v port mode)
     (write-id v '$ port mode))])

(struct ast:id:ref ast:id [(maybe-def #:mutable)]
 #:methods gen:custom-write
  [(define (write-proc v port mode)
     (write-id v '@ port mode))])

(define (ast-id-stxid aid)
  (cond [(ast:id? aid) (ast:id-stxid aid)]
        [(syntax? aid) aid]
        [(symbol? aid) (datum->syntax #f aid)]
        [else (error 'sham/sam/id "unknown identifier ~a" aid)]))
(define (ast-id-datum aid) (syntax-e (ast-id-stxid aid)))
(define (ast-id-gen aid)
  (match aid
    [(ast:id:ref md stxid defs) (ast:id:ref md (car (generate-temporaries (list stxid))) defs)]
    [(ast:id:def md stxid gen scope refs)
     (ast:id:def md (car (generate-temporaries (list stxid))) gen scope refs)]
    [else (error 'sham/sam/id "cannot gen id from: ~a" aid)]))

(define (add-id-ref! def ref)
  (when (ast:id:def? def)
    (set-ast:id:def-maybe-refs! def (cons ref (or (ast:id:def-maybe-refs def) '())))))

(define (id-create-ref def)
  (match-define (ast:id:def md stxid gen scope maybe-refs) def)
  (define ref (ast:id:ref md stxid def))
  (add-id-ref! def ref)
  ref)

(define (id-free=? i1 i2) (free-identifier=? (ast-id-stxid i1) (ast-id-stxid i2)))
(define (id-datum=? i1 i2) (equal? (ast-id-datum i1) (ast-id-datum i2)))

(define (id-orig=? i1 i2) (or (equal? (ast-id-stxid i1) (ast-id-stxid i2)) (id-free=? i1 i2)))

(define (id-def=? i1 i2)
  (and (ast:id:def? i1) (ast:id:def? i2) (id-orig=? i1 i2) (id-ref=? i1 i2)))
(define (id-ref=? i1 i2)
  (and (ast:id:ref? i1) (ast:id:ref? i2) (id-orig=? i1 i2) (id-ref=? i1 i2)))

(define (id=? i1 i2)
  (or (equal? i1 i2) (id-def=? i1 i2) (id-ref=? i1 i2)))
(define (any-id=? i1 i2)
  (or (id=? i1 i2)
      (and (and (ast:id? i1) (ast:id? i2))
           (or ;; (id-gen=? i1 i2)
               (id-orig=? i1 i2)))))

;; (define (cons-in-free-id-table fit ast-id)
;;   (free-id-table-update fit (ast-id-stxid ast-id) (λ (vs) (cons val vs)) '()))

;; ;; id-table: (hash-eqv ast:scope (free-id-table ast:id))
;; (define (make-id-table assocs)
;;   (define ((add-to-free-map stxid val) curr-val)
;;     (free-id-table-update curr-val stxid
;;                           (λ (vs) (cons (car vs) (cons val (cdr vs))))
;;                           '()))
;;   (let rec ([a assocs]
;;             [scope-mapping (make-immutable-hasheqv)])

;;     (define (add-mapping scope id val)
;;       (hash-update scope-mapping scope (add-to-free-map id val) (make-immutable-free-id-table)))
;;     (match-define (cons id val) a)
;;     (match id
;;       [(? syntax?) (add-mapping default-id-scope id id)]
;;       [(ast:id:def stxid gen scope maybe-ref) (add-mapping scope stxid id)]
;;       [(ast:id stxid) (add-mapping default-id-scope stxid id)])))

;; (define (id-table-add-def-id table def)
;;   (match-define (id-table scope-map) table)
;;   (id-table (add-id-table ))
;;   (match* (table def)
;;     [(id-table scope-map) (ast:id:def)]))

(define (find-def-for-id stxid id-map (scope default-id-scope))
  #f
  ;; (hash-ref (id-mapping-scope-mapping id-map) stxid)
  ;; (and (hash? maybe-scope-map)
  ;;      (hash-ref (hash-ref maybe-scope-map scope) stxid))
  )
