#lang racket

(require racket/generic
         racket/syntax
         syntax/parse/define
         (for-template racket))

(require "spec.rkt"
         (for-template
          (prefix-in rt: "runtime.rkt" )
          (prefix-in rt: "match.rkt")))

(provide (all-defined-out))

(define-generics ast-struct-constructor
  (construct-top-struct ast-struct-constructor ast-spec)
  (construct-group-struct ast-struct-constructor ast-spec group-spec)
  (construct-node-struct ast-struct-constructor ast-spec group-spec node-spec))

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
     (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes info) gs)
     (ast-struct-rkt gid-t (or parent syn-id) `() (reflection-name #``#,gsyn-id)))
   (define (construct-node-struct arc as gs ns)
     (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
     (match-define (ast:node (cons nid nid-t) nsyn-id nargs pat ninfo) ns)
     (ast-struct-rkt nid-t gid-t `() (reflection-name #``#,nsyn-id)))])

(struct rkt-term-type-constructor [spec]
  #:methods gen:ast-builder
  [(define (build-top-struct ab tstruct as) tstruct)
   (define (build-group-struct ab gstruct as gs) gstruct)
   (define (build-group-extra ab gextra as gs) gextra)
   (define (build-node-struct ab nstruct as gs ns) nstruct)
   (define (build-node-extra ab nextra as gs ns) nextra)])

(define-ast-builder (rkt-term-type spec)
  (define (build-group-extra ab gextra as gs)
    (match-define (ast id sid groups info) as)
    (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes info) gs)
    (cons #`(begin-for-syntax
              (define gsyn-id
                (rt:term-type #,gid-t rt:group-term-match-expander group-spec-id #,sid)))
          gextra))
  (define (build-node-extra ab nextra as gs ns)
    (match-define (ast id sid groups info) as)
    (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
    (match-define (ast:node (cons nid nid-t) nsyn-id nargs pat ninfo) ns)
    (cons #`(begin-for-syntax
              (define nsyn-id
                (rt:term-type #,nid-t rt:node-term-match-expander group-spec-id #,sid)))
          nextra)))
