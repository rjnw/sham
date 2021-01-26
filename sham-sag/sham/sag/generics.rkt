#lang racket

(require racket/generic
         racket/syntax
         syntax/parse/define
         (for-template racket))

(require "spec.rkt"
         (for-template (prefix-in rt: "runtime.rkt")))

(provide (all-defined-out))

(define-generics ast-struct-constructor
  (construct-top-struct ast-struct-constructor ast-spec)
  (construct-group-struct ast-struct-constructor ast-spec group-spec)
  (construct-node-struct ast-struct-constructor ast-spec group-spec node-spec)
  (pattern-match-expander ast-struct-constructor ast-pattern))

(define-generics ast-construct
  (->syntax ast-construct)
  #:defaults
  ([syntax?
    (define (->syntax c) c)]
   [list?
    (define/generic to-syntax ->syntax)
    (define (->syntax c)
      (map to-syntax (filter (compose not false?) c)))]))

;; extras is a list of syntax build-*-extra is called with a fold on the list
(define-generics ast-builder
  (build-top-struct ast-builder top-struct ast-spec)
  (build-group-struct ast-builder group-struct ast-spec group-spec)
  (build-group-extra ast-builder group-extra ast-spec group-spec)
  (build-node-struct ast-builder node-struct ast-spec group-spec node-spec)
  (build-node-extra ast-builder node-extra ast-spec group-spec node-spec)
  #:defaults
  ([any/c
    (define (build-top-struct ab tstruct as) tstruct)
    (define (build-group-struct ab gstruct as gs) gstruct)
    (define (build-group-extra ab gextra as gs) gextra)
    (define (build-node-struct ab nstruct as gs ns) nstruct)
    (define (build-node-extra ab nextra as gs ns) nextra)]))

#;(struct sample-ast-builder []
    #:methods gen:ast-builder
    [(define (build-top-struct ab tstruct as) tstruct)
     (define (build-group-struct ab gstruct as gs) gstruct)
     (define (build-group-extra ab gextra as gs) gextra)
     (define (build-node-struct ab nstruct as gs ns) nstruct)
     (define (build-node-extra ab nextra as gs ns) nextra)])

;; wrapper around make-struct-type arguments
(struct ast:struct:rkt [name maybe-parent fields options]
  #:methods gen:ast-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax asr)
     (match-define (ast:struct:rkt name maybe-parent fields options) asr)
     #`(struct #,name #,@(if maybe-parent (list maybe-parent) (list))
         #,(for/list ([a fields])
             (match a
               [(cons id options) #`(id #,options)]
               [(? syntax?) a]))
         #,@(flatten
             (for/list ([(id args) options])
               (let ([oid (datum->syntax #f (string->keyword (format "~a" id)))])
                 (cond [(list? args) (list* oid (to-syntax args))]
                       [(syntax? args) (list oid args)]))))))])

(define-simple-macro (ast-struct-rkt name (~optional maybe-parent)
                                     fields
                                     (~or* (option-ids:id option-vals) option-ids:id) ...)
  (ast:struct:rkt name (~? maybe-parent) fields (make-immutable-hash (list (cons (quote option-ids) option-vals) ...))))

(define (ast-struct-rkt-set-option asr option-id option-val)
  (match-define (ast:struct:rkt name maybe-parent fields options) asr)
  (ast:struct:rkt name maybe-parent fields (hash-set options option-id option-val)))

(define-simple-macro (update-ast-struct-rkt-option asr^ option-id option-value-f (~optional option-value-default))
  (let ([asr asr^]
        [oid (quote option-id)])
    (ast-struct-rkt-set-option asr oid (option-value-f (hash-ref (ast:struct:rkt-options asr) oid (~? option-value-default))))))

(struct rkt-ast-struct-constructor [spec]
  #:methods gen:ast-struct-constructor
  [(define (construct-top-struct arc as)
     (match-define (ast id sid groups info) as)
     (ast-struct-rkt sid #`rt:ast:term `() (reflection-name #``#,id)))
   (define (construct-group-struct arc as gs)
     (match-define (ast top-id syn-id groups top-info) as)
     (match-define (ast:group gid gsyn-id parent gargs nodes info) gs)
     (ast-struct-rkt gsyn-id (or parent syn-id) (map ast:basic-syn-id gargs)))
   (define (construct-node-struct arc as gs ns)
     (match-define (ast:group gid gsyn-id parent gargs nodes ginfo) gs)
     (match-define (ast:node nid nsyn-id nargs pat ninfo) ns)
     (ast-struct-rkt nsyn-id gsyn-id (map ast:basic-syn-id nargs)))
   (define (pattern-match-expander arc pat)

     #`(lambda (stx) #'42))])

(module+ test
  (define (match-expander pat stx)
    (printf "pat: ~a, stx: ~a\n" pat stx)
    (define (ooo? p)
      (define (sooo? s) (regexp-match #px"^(_|((__|\\.\\.)(_|\\.|[[:digit:]]+)))$" s))
      (and (identifier? p)
           (sooo? (symbol->string (syntax->datum p)))))
    (define (remove-datum pt) (filter (compose not ast:pat:datum?) pt))
    (if (ooo? pat)
        pat
        (match pat
          [(ast:pat:single id)
           (if (identifier? stx) stx (error 'stop))]
          [(ast:pat:datum syn) '()]
          ;; [(ast:pat:multipls specs)
          ;;  #:when (equal? (length specs) 1)]
          [(ast:pat:multiple pms)
           (let ([se (syntax-e stx)]
                 [ps (remove-datum pms)])
             (if (equal? (length se) (length ps))
                 (map match-expander ps se)
                 (error "pattern for multiple syntax values don't match in length")))]
          [(ast:pat:repeat pr)
           (map (curry match-expander pr) (syntax-e stx))]
          [(ast:pat:checker check id) stx])))
  (define pt
    (ast:pat:multiple
     (list
      (ast:pat:datum `letrec)
      (ast:pat:multiple
       (list
        (ast:pat:repeat
         (ast:pat:multiple
          (list (ast:pat:single #'ids) (ast:pat:single #'vals))))))
      (ast:pat:single #'e))))
  (pretty-print (match-expander pt #`((([a b] (... ...))) e)))
  )
