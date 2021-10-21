#lang racket

(require syntax/parse)
(provide (all-defined-out))

(define-splicing-syntax-class keyword-value
  (pattern (~seq k:keyword v:expr ...)
           #:attr spec (cons (string->symbol (keyword->string (syntax->datum #`k)))
                             (syntax->list #`(v ...)))))
(define-splicing-syntax-class keyword-info
  (pattern (~seq ki:keyword-value ...)
           #:attr spec (attribute ki.spec)))

;;; keyword info is stored in (list (cons keyword values) ...)
;;  combine duplicate key value pair into one pair
;;  ... (a . v0) ... (a . v1) ... => ... (a v0 v1) ...
(define ((combine (with identity)) ls) (cons (caar ls) (with (map cdr ls))))
(define (dedup-assoc i (cmb (combine)))
  (map cmb (group-by car i)))

(define (assoc-default key lst (dflt #f) (is-equal? equal?))
  (let ([v (assoc key lst is-equal?)])
    (if v (cdr v) dflt)))
(define info/c (listof (cons/c symbol? (listof syntax?))))

(define (info-value key lst (dflt #f))
  (let ([vs (assoc-default key lst dflt)]) (or vs dflt)))
(define (info-1value key lst (dflt #f))
  (match (info-value key lst dflt)
    [(list v) v]
    [v (or v dflt)]))
(define (info-values key lst)
  (match lst
    [(cons (cons ik iv) rst)
     (if (equal? ik key) (cons iv (info-values key rst)) (info-values key rst))]
    [else '()]))
(define (add-info key val inf) (cons (list key val) inf))
(define (remove-info key inf)
  (if (list? key)
      (foldr remove-info inf key)
      (filter (λ (p) (not (equal? key (car p)))) inf)))

(define (combine-info #:with (with (combine flatten)) . infs)
  (dedup-assoc (apply append infs) with))

;; applies `f` on the first assoc pair matching `key` in `lst`
;;   if not found call f with dflt
(define (update-info key f lst (dflt '()))
  (match lst
    ['() (list (cons key (f dflt)))]
    [(cons (cons ik iv) rst)
     #:when (equal? ik key)
     (cons (cons ik (f iv)) rst)]
    [(cons c rst) (cons c (update-info key f rst dflt))]))

(define (insert-info key val lst)
  (cond [(empty? lst) (list key val)]
        [(and (cons? lst) (equal? (caar lst) key))
         `((,(caar lst) ,val ,@(cdar lst)) . ,(cdr lst))]
        [else (cons (car lst) (insert-info key val (cdr lst)))]))

(define (pretty-print-info lst)
  (for ([v lst])
    (match v
      [(cons k v) (printf "\t~a: ~a\n" k v)])))

(define (default-metadata . specs)
  (cond
    [(ormap (curry info-1value 'default-metadata) specs) => car]
    [else #f]))

(define-syntax (kw-info stx)
  (define (do-val v)
    (syntax-case v ()
      [(vs ...) #`(list vs ...)]
      [i #`i]))
  (syntax-case stx ()
    [(_ (key . val) ...)
     (with-syntax ([(aval ...) (map do-val (syntax-e #`(val ...)))])
       #`(list (cons key aval) ...))]))

(module+ test
  (require rackunit)
  (define i1 `((a . 1) (b . 2) (c 1 2 3)))
  (define i2 `((a . 11) (a . 12) (b 21 22))))
