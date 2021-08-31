#lang racket
(require racket/generic)

(provide (all-defined-out))

(define-generics stx-construct
  (->syntax stx-construct)
  #:defaults
  ([syntax?
    (define (->syntax c) c)]
   [list?
    (define/generic to-syntax ->syntax)
    (define (->syntax c)
      (map to-syntax (filter (compose not false?) c)))]))

(module* ast #f

  (require racket/generic
           racket/syntax
           syntax/parse/define
           (for-template racket)
           (for-syntax racket/match
                       racket/syntax))
  (require "kw-info.rkt"
           (submod "spec.rkt" ast)
           (submod "runtime.rkt" ast)
           (prefix-in st: "transformer.rkt")
           (prefix-in ss: "storage.rkt")

           (for-template
            (prefix-in rt: "../runtime.rkt")
            (prefix-in rp: "../runtime/props.rkt")))
  (provide (all-defined-out))

  (define-generics ast-builder
    (update-others ast-builder builders)
    (update-spec ast-builder ast-spec)
    (build-top ast-builder top-constructs ast-spec)
    (build-group ast-builder group-constructs ast-spec group-spec)
    (build-node ast-builder node-constructs ast-spec group-spec node-spec)
    #:defaults
    ([any/c
      (define (update-others ab bs) bs)
      (define (update-spec ab as) as)
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
                               (define (update-spec ab as) as)
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
    #:methods gen:stx-construct
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

  (define (update-each-spec spec f)
    (let rec ([s spec])
      (match s
        [(ast rid id grps inf)
         (f (ast rid id (for/list ([g grps]) (cons (car g) (rec (cdr g)))) inf))]
        [(ast:group gid ginfo gparent gargs gnodes)
         (f (ast:group gid ginfo gparent gargs (for/list ([n gnodes]) (cons (car n) (rec (cdr n))))))]
        [(ast:node nid ninfo nargs npat) (f s)]
        [else (f s)])))
  (define (update-ids spec f)
    (match spec
      [(ast rid ids grps info) (ast rid (f spec ids) grps info)]
      [(ast:group gids ginfo gparent gargs gnodes)
       (ast:group (f spec gids) ginfo gparent gargs gnodes)]
      [(ast:node nids ninfo nargs npat)
       (ast:node (f spec nids) ninfo nargs npat)]
      [else spec]))
  ;; (add-id nids key (if value value (ast-custom-id spec key)))

  (define-ast-builder (rkt-struct)
    (update-spec
     (as)
     (define (upd spec)
       (update-ids spec
                   (λ (s ids)
                     (add-id 'rkt-struct (format-id (get-fid ids) "struct-~a" (get-fid ids)) ids))))
     (update-each-spec as upd))
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
    #:methods gen:stx-construct
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
       #`(begin (define (#,make-f #:md (md #,(default-metadata ninfo ginfo tinfo)) #,@garg-ids #,@narg-ids)
                  ;; TODO add optional arg for srcloc
                  (#,nid (rt:generic-metadata md) #,(ss:group-arg-storage garg-ids) #,(ss:node-args-storage nargs pat)))
                (define (#,f? term) (#,(format-id nid "~a?" nid) term))
                #,@(map node-accessor nargs arg-fs)))))

  (define-ast-builder (rkt-struct-functions)
    (build-group
     (gcs as gs)
     (match-define (ast tid tids groups tinfo) as)
     (match-define (ast:group gids ginfo parent gargs nodes) gs)
     (define group-arg-funcs
       (for/list ([garg gargs]
                  [i (in-range (sequence-length gargs))])
         #`(define (#,(format-id (get-fid gids) "~a-~a" (get-fid gids) (get-fid (get-ast-id garg))) term)
             (vector-ref (rt:ast:group-args term) #,i))))
     (cons #`(define (#,(format-id (get-fid gids) "~a?" (get-fid gids)) term)
               (#,(format-id (get-struct-id gids) "~a?" (get-struct-id gids)) term))
           (append group-arg-funcs gcs)))
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
    (update-spec
     (as)
     (define (upd spec) (update-ids spec (λ (s ids) (add-id 'spec (format-id (get-fid ids) "spec-~a" (get-fid ids)) ids))))
     (update-each-spec as upd))
    (build-group
     (gcs as gs)
     (match-define (ast tid tids groups tinfo) as)
     (match-define (ast:group gids ginfo parent gargs nodes) gs)
     (cons #`(define-for-syntax #,(get-sid gids) (find-group-spec #'#,(get-oid gids) #,(get-sid tids)))
           gcs))
    (build-node
     (ncs as gs ns)
     (match-define (ast tid tids groups tinfo) as)
     (match-define (ast:group gids ginfo parent gargs nodes) gs)
     (match-define (ast:node nids ninfo nargs pat) ns)
     (cons #`(define-for-syntax #,(get-sid nids)
               (find-node-spec #'#,(get-oid nids) #,(get-sid gids) #,(get-sid tids) ))
           ncs)))

  (define-ast-builder (rkt-term-type)
    (update-spec
     (as)
     (define (upd spec)
       (update-ids spec
                   (λ (s ids)
                     (add-id 'f
                             (cond
                               [(ast:group? s) (format-group-id as s)]
                               [(ast:node? s) (format-node-id as s)]
                               [(ast? s) (get-oid ids)])
                             ids))))
     (update-each-spec as upd))
    (build-group
     (gcs as gs)
     (match-define (ast tid tids groups tinfo) as)
     (match-define (ast:group gids ginfo parent gargs nodes) gs)
     (cons
      #`(define-syntax #,(get-fid gids)
          (term-type st:rkt-pattern-transformer st:rkt-match-expander #,(get-sid gids) #,(get-sid tids)))
      gcs))
    (build-node
     (ncs as gs ns)
     (match-define (ast tid tids groups tinfo) as)
     (match-define (ast:group gids ginfo parent gargs nodes) gs)
     (match-define (ast:node nids ninfo nargs pat) ns)
     (cons #`(define-syntax #,(get-fid nids)
               (term-type st:rkt-pattern-transformer st:rkt-match-expander #,(get-sid nids) #,(get-sid tids)))
           ncs)))

  (define (default-rkt-struct-builder)
    (list
     (rkt-struct-prop-builder)
     (rkt-struct-functions-builder)
     (rkt-struct-builder)
     (ast-spec-builder)
     (rkt-term-type-builder))))

(module* compiler racket
  (require racket/generic)
  (provide (all-defined-out))
  (define-generics cmplr-spec-builder
    (update-cmplr-spec cmplr-spec-builder cmplr-spec))
  (define-generics cmplr-group-builder
    (build-cmplr-group cmplr-group-builder group-constructs cmplr-spec cmplr-group-spec))
  (define-generics cmplr-node-builder
    (build-cmplr-node cmplr-node-builder node-construct cmplr-spec cmplr-group-spec cmplr-node-spec))
  (define-generics cmplr-top-builder
    (build-cmplr-top cmplr-top-builder top-construct cmplr-spec))
  (define (update-spec builder spec)
    (if (cmplr-spec-builder? builder) (update-cmplr-spec builder spec) spec))
  (define ((build-group cmplr-spec group-spec) builder group-construct)
    (if (cmplr-group-builder? builder)
        (build-cmplr-group builder group-construct cmplr-spec group-spec)
        group-construct))
  (define ((build-node cmplr-spec group-spec node-spec) builder node-construct)
    (if (cmplr-node-builder? builder)
        (build-cmplr-node builder node-construct cmplr-spec group-spec node-spec)
        node-construct))
  (define ((build-top cmplr-spec) builder top-construct)
    (if (cmplr-top-builder? builder)
        (build-cmplr-top builder top-construct cmplr-spec)
        top-construct))

  (define-generics cmplr-node-pattern
    (node-operation-id cmplr-node-pattern)
    (parse-node-binding cmplr-node-pattern stx path cmplr-spec group-spec node-spec))
  (define-generics cmplr-bind-pattern
    (bound-variables cmplr-bind-pattern)
    (match-syntax cmplr-bind-pattern))

  (define-generics cmplr-pattern
    (expand-pattern cmplr-pattern stx input-zipper)
    (perform-pattern cmplr-pattern stx output-zipper))

  (define-generics cmplr-bind-operator
    (bind-operator-identifier cmplr-bind-operator)
    (bind-operator-parse-syntax cmplr-bind-operator body-stx ast-type)
    (bind-operator-gen-syntax cmplr-bind-operator bound-var state))

  (define-generics cmplr-body-operator
    (body-operator-identifier cmplr-body-operator)
    (body-operator-gen-syntax cmplr-body-operator rst-stx frec state))
  )
