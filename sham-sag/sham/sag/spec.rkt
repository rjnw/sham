#lang racket
(require (prefix-in prv: "private/spec.rkt")
         (submod "private/spec.rkt" pattern))
(provide (all-defined-out)
         (all-from-out (submod "private/spec.rkt" pattern)))

(struct ast (id sid groups info)
  #:property prop:rename-transformer 1)

(struct ast:group (id syn-id parent args-assoc nodes info) #:prefab)
(struct ast:node (id syn-id args-assoc pattern info) #:prefab)

(struct ast:type () #:prefab)
(struct ast:type:metadata ast:type () #:prefab)

(define (group-nodes gs)
  (hash-values (ast:group-nodes gs)))
(define (lookup-group-spec group-id ast-spec)
  (cond [(syntax? group-id) (hash-ref (ast-groups ast-spec) (syntax->datum group-id))]
        [(symbol? group-id) (hash-ref (ast-groups ast-spec) group-id)]
        [else #f]))

(define (spec->storage spec)
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
    (hash-storage info datum-storage info-value))
  (define (type-storage type) #`#f)
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
                  #,(assoc-storage nargs syntax-storage type-storage)
                  #,(pattern-storage npat)
                  #,(info-storage ninfo)))
    #`(ast:group #'#,gid #'#,gsyn #'#,gparent
                 #,(assoc-storage gargs syntax-storage type-storage)
                 #,(hash-storage gnodes syntax-storage node-storage)
                 #,(info-storage ginfo)))
  (match-define (ast id sid groups info) spec)
  #`(ast #'#,id #'#,sid
         #,(hash-storage groups syntax-storage group-storage)
         #,(info-storage info)))
