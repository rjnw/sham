#lang racket

(require racket/splicing)

(require "private/utils.rkt"
         (submod "private/spec.rkt" pattern))

(provide (all-defined-out)
         (all-from-out (submod "private/spec.rkt" pattern)))

(struct ast (rename-id id groups info)
  #:property prop:rename-transformer 1)

(struct ast:id (orig gen form) #:prefab)
(struct ast:basic (id) #:prefab)
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

(define ast:pat/c
  (flat-rec-contract pat/c
                     (struct/c ast:pat:datum symbol?)
                     (struct/c ast:pat:single (maybe/c procedure?) (maybe/c symbol?))
                     (struct/c ast:pat:multiple (listof pat/c))
                     (struct/c ast:pat:repeat pat/c (maybe/c natural-number/c))))

(define ast:id/c (struct/c ast:id syntax? syntax? syntax?))
(define ast:info/c (listof (cons/c symbol? (listof syntax?))))
(define ast:type/c ast:type?)
(define ast:node/c (struct/c ast:node
                             ast:id/c
                             (listof (struct/c ast:node:arg ast:id/c ast:type/c ast:info/c))
                             ast:pat/c
                             ast:info/c))
(define ast:group/c (struct/c ast:group
                              ast:id/c
                              syntax?
                              (listof (struct/c ast:group:arg ast:id/c ast:type/c ast:info/c))
                              (assoc/c symbol? ast:node/c)
                              ast:info/c))
(define ast/c (struct/c ast syntax? ast:id/c (assoc/c symbol? ast:group/c) ast:info/c))

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
      [(ast:group gid prnt args nds inf)
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
  (define (id-storage id)
    (match id
      [(ast:id orig temp form)
       #`(ast:id #,(syntax-storage orig)
                 #,(syntax-storage temp)
                 #,(syntax-storage form))]))
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
      [(ast:group:arg id type info)
       #`(ast:group:arg #,(id-storage id)
                        #,(type-storage type)
                        #,(info-storage info))]
      [(ast:node:arg id type info)
       #`(ast:node:arg #,(id-storage id)
                       #,(type-storage type)
                       #,(info-storage info))]))

  (define (group-storage group)
    (match-define (ast:group gid gparent gargs gnodes ginfo) group)
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
      (match-define (ast:node nid nargs npat ninfo) node)
      #`(ast:node #,(id-storage nid)
                  #,(list-storage nargs arg-storage)
                  #,(pattern-storage npat)
                  #,(info-storage ninfo)))
    #`(ast:group #,(id-storage gid) '#,gparent
                 #,(list-storage gargs arg-storage)
                 #,(assoc-storage gnodes datum-storage node-storage)
                 #,(info-storage ginfo)))
  (match-define (ast id tid groups info) spec)
  #`(ast #'#,id #,(id-storage tid)
         #,(assoc-storage groups datum-storage group-storage)
         #,(info-storage info)))

(define (pretty-info info)
  (for/list ([ip info])
    (cons (car ip) (map syntax->datum (flatten (cdr ip))))))
(define (pretty-id id)
  (match id
    [(ast:id id tid fid) `(,(syntax-e id) ,(syntax-e fid))]))
(define (pretty-arg arg)
  (match arg
    [(ast:basic id) (pretty-id id)]))
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
  (match-define (ast:node nid nargs npat ninfo) node)
  `(,(pretty-id nid)
    ,@(map pretty-arg nargs)
    ,(pretty-pattern npat)
    ,(pretty-info ninfo)))
(define (pretty-group grp)
  (match-define (ast:group gid gparent gargs gnodes ginfo) grp)
  `(,(pretty-id gid) ,(syntax-e gparent)
                     ,@(map pretty-arg gargs)
                     ,@(for/list ([np gnodes])
                         `((#:node ,(car np)) ,(pretty-node (cdr np))))
                     ,(pretty-info ginfo)))

(define (pretty-spec spec)
  (match-define (ast id sid groups info) spec)

  `((#:ast ,(syntax-e id) ,(pretty-id sid))
    ,@(for/list ([gp groups])
        `((#:group  ,(car gp)) ,(pretty-group (cdr gp))))
    ,(pretty-info info)))
