#lang racket
(require (prefix-in prv: "private/spec.rkt")
         (submod "private/spec.rkt" pattern))
(provide (all-defined-out)
         (all-from-out (submod "private/spec.rkt" pattern)))

(struct ast (id syn-id groups info)
  #:property prop:rename-transformer 1)

(struct ast:basic (id syn-id) #:prefab)
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
  (hash-values (ast:group-nodes gs)))
(define (lookup-group-spec group-id ast-spec)
  (cond [(syntax? group-id) (hash-ref (ast-groups ast-spec) (syntax->datum group-id))]
        [(symbol? group-id) (hash-ref (ast-groups ast-spec) group-id)]
        [else #f]))

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
        (hash-storage info datum-storage info-value)
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
    (match-define (ast:group gid gsyn gparent gargs gnodes ginfo) group)
    (define (node-storage node)
      (define (pattern-storage pattern)
        (match pattern
          [(ast:pat:single type id)
           #`(ast:pat:single #,(if type #'#,type type) #'#,id)]
          [(ast:pat:datum syn)
           #`(ast:pat:datum `#,syn)]
          [(ast:pat:multiple specs)
           #`(ast:pat:multiple (list #,@(map pattern-storage specs)))]
          [(ast:pat:repeat spec)
           #`(ast:pat:repeat #,(pattern-storage spec))]
          [(ast:pat:checker check id)
           #`(ast:pat:checker #'#,check #'#,id)]))
      (match-define (ast:node nid nsyn nargs npat ninfo) node)
      #`(ast:node #'#,nid #'#,nsyn
                  #,(list-storage nargs arg-storage)
                  #,(pattern-storage npat)
                  #,(info-storage ninfo)))
    #`(ast:group #'#,gid #'#,gsyn #'#,gparent
                 #,(list-storage gargs arg-storage)
                 #,(hash-storage gnodes syntax-storage node-storage)
                 #,(info-storage ginfo)))
  (match-define (ast id sid groups info) spec)
  #`(ast #'#,id #'#,sid
         #,(hash-storage groups syntax-storage group-storage)
         #,(info-storage info)))
