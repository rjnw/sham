#lang racket

(require racket/generic
         racket/syntax
         syntax/parse/define
         (for-template racket)
         (for-syntax racket/match
                     racket/syntax))

(require "syntax/spec.rkt"
         (prefix-in srt: "syntax/runtime.rkt")
         (prefix-in st: "syntax/transformer.rkt")
         (prefix-in sm: "syntax/match.rkt")
         (prefix-in ss: "syntax/storage.rkt")
         (for-template
          (prefix-in rt: "runtime.rkt")))

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

(define-syntax (define-ast-builder stx)
  (syntax-parse stx
    [(_ (sn-raw:id args:id ...)
        (gf:id (gf-args:id ...)
               gf-body:expr ...)
        ...)
     #:with sn (format-id #`sn-raw "~a-builder" #`sn-raw)
     #`(struct sn [args ...]
         #:methods gen:ast-builder
         #,(let [(given-assoc (map cons
                                   (syntax->list #`(gf ...))
                                   (map list
                                        (syntax->list #`((gf-args ...) ...))
                                        (syntax->list #`((gf-body ...) ...)))))]
             (for/list [(og-gen
                         (syntax->list
                          #`((define (build-top-struct ab tstruct as) tstruct)
                             (define (build-group-struct ab gstruct as gs) gstruct)
                             (define (build-group-extra ab gextra as gs) gextra)
                             (define (build-node-struct ab nstruct as gs ns) nstruct)
                             (define (build-node-extra ab nextra as gs ns) nextra))))]
               (syntax-parse og-gen
                 [(_ (og-name og-args ...) og-body ...)
                  (let [(maybe-given (assoc #`og-name given-assoc
                                            free-identifier=?))]
                    (if maybe-given
                        (match (cdr maybe-given)
                          [(list given-args given-body)
                           (with-syntax [((gargs ...) given-args)
                                         ((gbody ...) given-body)]
                             (syntax/loc (car maybe-given)
                               (define (og-name ab gargs ...)
                                 (match-define (sn args ...) ab)
                                 gbody ...)))])
                        og-gen))]))))]))

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

(define-simple-macro
  (ast-struct-rkt name (~optional maybe-parent)
                  fields
                  (~or* (option-ids:id option-vals) option-ids:id) ...)
  (ast:struct:rkt name
                  (~? maybe-parent)
                  fields
                  (make-immutable-hash
                   (list (cons (quote option-ids) option-vals) ...))))

(define (ast-struct-rkt-set-option asr option-id option-val)
  (match-define (ast:struct:rkt name maybe-parent fields options) asr)
  (ast:struct:rkt name maybe-parent fields (hash-set options option-id option-val)))

(define-simple-macro
  (update-ast-struct-rkt-option asr^ option-id option-value-f (~optional option-value-default))
  (let ([asr asr^]
        [oid (quote option-id)])
    (ast-struct-rkt-set-option
     asr oid
     (option-value-f (hash-ref (ast:struct:rkt-options asr) oid (~? option-value-default))))))

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

(struct rkt-struct-node-functions [fids spec gspec nspec]
  #:methods gen:ast-construct
  ((define/generic to-syntax ->syntax)
   (define (->syntax rsnf)
     (match-define (rkt-struct-node-functions fids spec gspec nspec) rsnf)
     (match-define (ast top-id syn-id groups top-info) spec)
     (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes info) gspec)
     (match-define (ast:node (cons nid nid-t) nsyn-id nargs pat ninfo) nspec)
     (match-define (cons make-f arg-fs) fids)
     (define narg-ids (map (compose car ast:basic-id-tpair) nargs))
     (define garg-ids (map (compose car ast:basic-id-tpair) gargs))
     (define (node-accessor arg fid)
       #`(define (#,fid #,nid)
           #,((ss:from-node-storage arg pat) #`(rt:ast:term-vals #,nid))))
     #`(begin (define (#,make-f #:md (md #f) #,@garg-ids #,@narg-ids)
                (#,nid-t md #,(ss:group-arg-storage gargs) #,(ss:node-args-storage nargs pat)))
              #,@(map node-accessor nargs arg-fs)))))

(define-ast-builder (rkt-struct-functions spec)
  (build-node-extra
   (nextra as gs ns)
   (match-define (ast id sid groups info) as)
   (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
   (match-define (ast:node (cons nid nid-t) nsyn-id nargs pat ninfo) ns)
   (cons (rkt-struct-node-functions
          (cons (format-id nid "make-~a" nid)
                (map (lambda (arg)
                       (let ([arg-id (car (ast:basic-id-tpair arg))])
                         (format-id arg-id "~a-~a"
                                    nid (car (ast:basic-id-tpair arg)))))
                     nargs))
          as gs ns)
         nextra)))

(define-ast-builder (ast-spec spec)
  (build-group-extra
   (gextra as gs)
   (match-define (ast id sid groups info) as)
   (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
   (cons #`(define-for-syntax #,gid-t (lookup-group-spec #'#,gid #,sid))
         gextra))
  (build-node-extra
   (nextra as gs ns)
   (match-define (ast id sid groups info) as)
   (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
   (match-define (ast:node (cons nid nid-t) nsyn-id nargs pat ninfo) ns)
   (cons #`(define-for-syntax #,nid-t (lookup-node-spec #'#,nid #,gid-t #,sid))
         nextra)))

(define-ast-builder (rkt-term-type spec)
  (build-group-extra
   (gextra as gs)
   (match-define (ast id sid groups info) as)
   (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
   (define gen-id (generate-temporary gid))
   (append
    (list
     #`(define-syntax #,gsyn-id
         (srt:term-type st:rkt-pattern-transformer sm:term-match-expander #,gid-t #,sid)))
    gextra))
  (build-node-extra
   (nextra as gs ns)
   (match-define (ast id sid groups info) as)
   (match-define (ast:group (cons gid gid-t) gsyn-id parent gargs nodes ginfo) gs)
   (match-define (ast:node (cons nid nid-t) nsyn-id nargs pat ninfo) ns)
   (match (findf rkt-transformer? nextra)
     [(rkt-transformer tid ns^)
      (cons #`(define-syntax #,nsyn-id
                (srt:term-type tid sm:term-match-expander #,nid-t #,sid))
            nextra)]
     [else
      (cons #`(define-syntax #,nsyn-id
                (srt:term-type st:rkt-pattern-transformer
                               sm:term-match-expander #,nid-t #,sid))
            nextra)])))

(define (default-rkt-struct-constructor spec)
  (list (rkt-ast-struct-constructor spec)
        (ast-spec-builder spec)
        (rkt-term-type-builder spec)))
