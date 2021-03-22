#lang racket

(require racket/splicing)

(require (prefix-in prv: "private/spec.rkt")
         (submod "private/spec.rkt" pattern))

(provide (all-defined-out)
         (all-from-out (submod "private/spec.rkt" pattern)))

(struct ast (id syn-id groups info)
  #:property prop:rename-transformer 1)

(struct ast:basic (id-tpair formatted-id) #:prefab) ;; id-tpair: (cons orig-id (generate-temporary orig-id))
(struct ast:group ast:basic (parent args nodes info) #:prefab)
(struct ast:group:arg ast:basic (type info) #:prefab)
(struct ast:node ast:basic (args pattern info) #:prefab)
(struct ast:node:arg ast:basic (type info) #:prefab)

(struct ast:type () #:prefab)
(struct ast:type:metadata ast:type () #:prefab)
(struct ast:type:internal ast:type (of depth) #:prefab)
(struct ast:type:external ast:type (of depth) #:prefab)
(struct ast:type:identifier ast:type () #:prefab)
(struct ast:type:unboxed ast:type (of) #:prefab)


(define (group-nodes gs)
  (map cdr (ast:group-nodes gs)))

(splicing-let ([forced-datum
                (λ (s)
                  (cond [(syntax? s) (syntax->datum s)]
                        [(string? s) (string->symbol s)]
                        [(symbol? s) s]
                        [else (error 'sham:sag:syntax-runtime "couldn't force to datum ~a" s)]))])
  (define (lookup-group-spec group-id ast-spec)
    (cdr (assoc (forced-datum group-id) (ast-groups ast-spec))))
  (define (lookup-node-spec node-id group-spec/id ast-spec)
    (match group-spec/id
      [(? (or/c syntax? symbol?))
       (lookup-node-spec node-id (lookup-group-spec group-spec/id ast-spec) ast-spec)]
      [(ast:group idt fid prnt args nds inf)
       (cdr (assoc (forced-datum node-id) nds))])))

(define (spec->storage spec)
  (define (list-storage l item-storage)
    #`(list #,@(map item-storage l)))
  (define (assoc-storage a key-storage value-storage)
    #`(list #,@(for/list ([v a])
                 (match-define (cons key value) v)
                 #`(cons #,(key-storage key) #,(value-storage value)))))
  (define (hash-storage h key-storage value-storage)
    #`(make-hash (list #,@(for/list ([(key value) h])
                            #`(cons #,(key-storage key) #,(value-storage value))))))
  (define (datum-storage v) #``#,v)
  (define (syntax-storage v) #`#'#,v)
  (define (info-storage info)
    (define (info-value v)
      (cond [(syntax? v) #`#'#,v]
            [(symbol? v) #`'#,v]
            [(list? v) #`(list #,@(map info-value v))]
            [(false? v) #`#f]))
    (if info
        (assoc-storage info datum-storage info-value)
        #`#f))
  (define (type-storage type) #`#f)
  (define (arg-storage a)
    (match a
      [(ast:group:arg id syn-id type info)
       #`(ast:group:arg #,(syntax-storage id)
                        #,(syntax-storage syn-id)
                        #,(type-storage type)
                        #,(info-storage info))]
      [(ast:node:arg id syn-id type info)
       #`(ast:node:arg #,(syntax-storage id)
                       #,(syntax-storage syn-id)
                       #,(type-storage type)
                       #,(info-storage info))]))

  (define (group-storage group)
    (match-define (ast:group (cons gid gid-t) gsyn gparent gargs gnodes ginfo) group)
    (define (node-storage node)
      (define (pattern-storage pattern)
        (match pattern
          [(ast:pat:single c id)        ;;TODO check for #f for c,id
           #`(ast:pat:single #'#,c #'#,id)]
          [(ast:pat:datum syn)
           #`(ast:pat:datum `#,syn)]
          [(ast:pat:multiple specs)
           #`(ast:pat:multiple (vector-immutable #,@(for/list ([s specs]) (pattern-storage s))))]
          [(ast:pat:repeat spec k)
           #`(ast:pat:repeat #,(pattern-storage spec) #,k)]))
      (match-define (ast:node (cons nid nid-t) nsyn nargs npat ninfo) node)
      #`(ast:node (cons #'#,nid #'#,nid-t) #'#,nsyn
                  #,(list-storage nargs arg-storage)
                  #,(pattern-storage npat)
                  #,(info-storage ninfo)))
    #`(ast:group (cons #'#,gid #'#,gid-t) #'#,gsyn #'#,gparent
                 #,(list-storage gargs arg-storage)
                 #,(assoc-storage gnodes datum-storage node-storage)
                 #,(info-storage ginfo)))
  (match-define (ast id sid groups info) spec)
  #`(ast #'#,id #'#,sid
         #,(assoc-storage groups datum-storage group-storage)
         #,(info-storage info)))

(define (pretty-info info)
  (for/list ([ip info])
    (cons (car ip) (map syntax->datum (flatten (cdr ip))))))
(define (pretty-arg arg)
  (match arg
    [(ast:basic id syn-id) `(,(syntax-e id) ,(syntax-e syn-id))]))
(define (pretty-pattern pattern)
  (match pattern
    [(ast:pat:single c id)
     `(#:single ,c ,(syntax-e id))]
    [(ast:pat:datum syn)
     `(#:datum ,syn)]
    [(ast:pat:multiple specs)
     `(#:multiple ,(vector-map pretty-pattern specs))]
    [(ast:pat:repeat spec n)
     `(#:repeat ,(pretty-pattern spec) ,n)]))
(define (pretty-node node)
  (match-define (ast:node nid nsyn nargs npat ninfo) node)
  `(,(syntax-e nsyn)
    ,@(map pretty-arg nargs)
    ,(pretty-pattern npat)
    ,(pretty-info ninfo)))
(define (pretty-group grp)
  (match-define (ast:group gid gsyn gparent gargs gnodes ginfo) grp)
  `(,(syntax-e gsyn) ,(syntax-e gparent)
                     ,@(map pretty-arg gargs)
                     ,@(for/list ([np gnodes])
                         `((#:node ,(car np)) ,(pretty-node (cdr np))))
                     ,(pretty-info ginfo)))

(define (pretty-spec spec)
  (match-define (ast id sid groups info) spec)

  `((#:ast ,(syntax-e id) ,(syntax-e sid))
    ,@(for/list ([gp groups])
        `((#:group  ,(car gp)) ,(pretty-group (cdr gp))))
    ,(pretty-info info)))
