#lang racket

(require racket/generic
         racket/syntax
         syntax/parse/define
         (for-template racket))

(require "spec.rkt"
         (for-template (prefix-in rt: "runtime.rkt")))

(provide (all-defined-out))

(define-generics ast-constructor
  (construct-top-struct ast-constructor ast-spec)
  (construct-group-struct ast-constructor ast-spec group-spec)
  (construct-node-struct ast-constructor ast-spec group-spec node-spec))

(define-generics ast-construct
  (->syntax ast-construct)
  #:defaults
  ([syntax?
    (define (->syntax c) c)]
   [list?
    (define/generic to-syntax ->syntax)
    (define (->syntax c)
      (printf "default:->syntax ~a\n" c)
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

;; wrapper around make-struct-type arguments
(struct ast:struct:rkt [name maybe-parent fields options]
  #:methods gen:ast-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax asr)
     (match-define (ast:struct:rkt name maybe-parent fields options) asr)
     (printf "->syntax ~a ~a ~a ~a\n" name maybe-parent fields options)
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

(define-simple-macro (ast-struct-rkt name (~optional maybe-parent) fields (option-ids option-vals) ...)
  (ast:struct:rkt name (~? maybe-parent) fields (make-immutable-hash (list (cons (quote option-ids) option-vals) ...))))

(define (ast-struct-rkt-set-option asr option-id option-val)
  (match-define (ast:struct:rkt name maybe-parent fields options) asr)
  (ast:struct:rkt name maybe-parent fields (hash-set options option-id option-val)))

(define-simple-macro (update-ast-struct-rkt-option asr^ option-id option-value-f (~optional option-value-default))
  (let ([asr asr^]
        [oid (quote option-id)])
    (ast-struct-rkt-set-option asr oid (option-value-f (hash-ref (ast:struct:rkt-options asr) oid (~? option-value-default))))))

(struct rkt-ast-constructor [spec]
  #:methods gen:ast-constructor
  [(define (construct-top-struct arc as)
     (match-define (ast id sid groups info) as)
     (ast-struct-rkt sid #`rt:ast `() (reflection-name #``#,id)))
   (define (construct-group-struct arc as gs)
     (match-define (ast top-id syn-id groups top-info) as)
     (match-define (ast:group gid gsyn-id parent gargs-assoc nodes info) gs)
     (ast-struct-rkt gsyn-id (or parent syn-id) (map car gargs-assoc)))
   (define (construct-node-struct arc as gs ns)
     (match-define (ast:group gid gsyn-id parent gargs-assoc nodes ginfo) gs)
     (match-define (ast:node nid nsyn-id nargs-assoc pat ninfo) ns)
     (ast-struct-rkt nsyn-id gsyn-id (map car nargs-assoc)))])
