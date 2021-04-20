#lang racket

(require racket/generic
         racket/syntax
         syntax/parse/define
         (for-template racket)
         (for-syntax racket/match
                     racket/syntax))

(require "syntax/spec.rkt"
         (prefix-in sr: "syntax/runtime.rkt")
         (prefix-in st: "syntax/transformer.rkt")
         (prefix-in ss: "syntax/storage.rkt")
         (for-template
          (prefix-in rt: "runtime.rkt")
          (prefix-in rp: "runtime/props.rkt")))

(provide (all-defined-out))

(define-generics ast-construct
  (->syntax ast-construct)
  #:defaults
  ([syntax?
    (define (->syntax c) c)]
   [list?
    (define/generic to-syntax ->syntax)
    (define (->syntax c)
      (map to-syntax (filter (compose not false?) c)))]))

(define-generics ast-builder
  (update-others ast-builder builders)
  (build-spec ast-builder ast-spec)
  (build-top ast-builder top-constructs ast-spec)
  (build-group ast-builder group-constructs ast-spec group-spec)
  (build-node ast-builder node-constructs ast-spec group-spec node-spec)
  #:defaults
  ([any/c
    (define (update-others ab bs) bs)
    (define (build-spec ab as) as)
    (define (build-top ab tcs as) tcs)
    (define (build-group ab gcs as gs) gcs)
    (define (build-node ab ncs as gs ns) ncs)]))

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
                          #`((define (update-others ab bs) bs)
                             (define (build-spec ab as) as)
                             (define (build-top ab tconstruct as) tconstruct)
                             (define (build-group ab gconstruct as gs) gconstruct)
                             (define (build-node ab nconstruct as gs ns) nconstruct))))]
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
;; utils for builders
(define (update pred? lst f) (map (lambda (v) (if (pred? v) (f v) v)) lst))

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
             (for/list ([opt options])
               (match-define (cons id args) opt)
               (let ([oid (datum->syntax #f (string->keyword (format "~a" id)))])
                 (cond [(list? args) (list* oid (to-syntax args))]
                       [(syntax? args) (list oid (to-syntax args))]))))))])

(define-simple-macro
  (ast-struct-rkt name (~optional maybe-parent)
                  fields
                  (~or* (option-ids:id option-vals) option-ids:id) ...)
  (ast:struct:rkt name
                  (~? maybe-parent)
                  fields
                  (list (cons (quote option-ids) option-vals) ...)))

(define (ast-struct-rkt-add-option asr option-id option-val)
  (match-define (ast:struct:rkt name maybe-parent fields options) asr)
  (ast:struct:rkt name maybe-parent fields (cons (cons option-id option-val) options)))

;; (define-simple-macro
;;   (update-ast-struct-rkt-option asr^ option-id option-value-f (~optional option-value-default))
;;   (let ([asr asr^]
;;         [oid (quote option-id)])
;;     (ast-struct-rkt-set-option
;;      asr oid
;;      (option-value-f (hash-ref (ast:struct:rkt-options asr) oid (~? option-value-default))))))

(define-ast-builder (rkt-struct)
  (build-top
   (tcs as)
   (match-define (ast tid tids groups tinfo) as)
   (cons (ast-struct-rkt (get-struct-id tids) #`rt:ast:term `() (reflection-name #``#,tid)) tcs))
  (build-group
   (gcs as gs)
   (match-define (ast tid tids groups top-info) as)
   (match-define (ast:group gids ginfo gpid gargs nodes) gs)
   (define parent (get-struct-id (if gpid (ast:basic-id (find-group-spec gpid as)) tids)))
   (cons (ast-struct-rkt (get-struct-id gids) parent `() (reflection-name #``#,(get-fid gids))) gcs))
  (build-node
   (ncs as gs ns)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (match-define (ast:node nids ninfo nargs pat) ns)
   (cons (ast-struct-rkt (get-struct-id nids) (get-struct-id gids) `() (reflection-name #``#,(get-fid nids))) ncs)))

(struct rkt-struct-node-functions [fids spec gspec nspec]
  #:methods gen:ast-construct
  ((define/generic to-syntax ->syntax)
   (define (->syntax rsnf)
     (match-define (rkt-struct-node-functions fids spec gspec nspec) rsnf)
     (match-define (ast tid tids groups tinfo) spec)
     (match-define (ast:group gids ginfo parent gargs nodes) gspec)
     (match-define (ast:node nids ninfo nargs pat) nspec)
     (define nid (get-struct-id nids))
     (define nido (get-oid nids))
     (match-define `(,make-f ,f? . ,arg-fs) fids)
     (define narg-ids (map (compose get-fid get-ast-id) nargs))
     (define garg-ids (map (compose get-fid get-ast-id) gargs))
     (define (node-accessor arg fid)
       #`(define (#,fid #,nido)
           #,((ss:from-node-storage arg pat) #`(rt:ast:term-args #,nido))))
     #`(begin (define (#,make-f #:md (md #f) #,@garg-ids #,@narg-ids)
                (#,nid md #,(ss:group-arg-storage gargs) #,(ss:node-args-storage nargs pat)))
              (define (#,f? term) (#,(format-id nid "~a?" nid) term))
              #,@(map node-accessor nargs arg-fs)))))

(define-ast-builder (rkt-struct-functions)
  (build-group
   (gcs as gs)
   (match-define (ast tid tids groups tinfo) as)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (cons #`(define (#,(format-id (get-fid gids) "~a?" (get-fid gids)) term)
             (#,(format-id (get-struct-id gids) "~a?" (get-struct-id gids)) term))
         gcs))
  (build-node
   (ncs as gs ns)
   (match-define (ast tid tids groups tinfo) as)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (match-define (ast:node nids ninfo nargs pat) ns)
   (define nid (get-struct-id nids))
   (define nido (get-fid nids))
   (cons (rkt-struct-node-functions
          (list* (format-id nido "make-~a" nido)
                 (format-id nido "~a?" nido)
                 (map (lambda (arg)
                        (let ([arg-id (get-fid (get-ast-id arg))])
                          (format-id arg-id "~a-~a" nido arg-id)))
                      nargs))
          as gs ns)
         ncs)))

(define-ast-builder (rkt-struct-prop)
  (build-node
   (ncs as gs ns)
   (match-define (ast:node nids ninfo nargs pat) ns)
   (define (add-props asr)
     (ast-struct-rkt-add-option asr 'property (list #'rp:prop:ast-constructor #`(lambda () #,(get-struct-id nids)))))
   ;; (map (lambda (c) (printf "c: ~a, ~a\n" (ast:struct:rkt? c) c)) ncs)
   (update ast:struct:rkt? ncs add-props)))

(define-ast-builder (ast-spec)
  (build-group
   (gcs as gs)
   (match-define (ast tid tids groups tinfo) as)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (cons #`(define-for-syntax #,(get-tid gids) (find-group-spec #'#,(get-oid gids) #,(get-tid tids)))
         gcs))
  (build-node
   (ncs as gs ns)
   (match-define (ast tid tids groups tinfo) as)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (match-define (ast:node nids ninfo nargs pat) ns)
   (cons #`(define-for-syntax #,(get-tid nids)
             (find-node-spec #'#,(get-oid nids) #,(get-tid gids) #,(get-tid tids) ))
         ncs)))

(define-ast-builder (rkt-term-type)
  (build-group
   (gcs as gs)
   (match-define (ast tid tids groups tinfo) as)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (cons
    #`(define-syntax #,(get-fid gids)
        (sr:term-type st:rkt-pattern-transformer st:rkt-match-expander #,(get-tid gids) #,(get-tid tids)))
    gcs))
  (build-node
   (ncs as gs ns)
   (match-define (ast tid tids groups tinfo) as)
   (match-define (ast:group gids ginfo parent gargs nodes) gs)
   (match-define (ast:node nids ninfo nargs pat) ns)
   (cons #`(define-syntax #,(get-fid nids)
             (sr:term-type st:rkt-pattern-transformer st:rkt-match-expander #,(get-tid nids) #,(get-tid tids)))
         ncs)))

(define (default-rkt-struct-builder)
  (list
   (rkt-struct-prop-builder)
   (rkt-struct-builder)
   (ast-spec-builder)
   (rkt-term-type-builder)))
