#lang racket

(require racket/syntax
         syntax/parse)

(require "type.rkt"
         "format.rkt"
         "private/utils.rkt"
         "private/syntax-class.rkt"
         (prefix-in prv: "private/spec.rkt")
         (submod "private/spec.rkt" pattern)
         (submod "private/spec.rkt" type))

(provide (all-defined-out)
         (all-from-out
          (submod "private/spec.rkt" pattern)
          (submod "private/spec.rkt" type)))

(struct ast (rename-id id groups info)
  #:property prop:rename-transformer 1)

(struct ast:basic (id info) #:prefab)
(struct ast:group ast:basic (parent args nodes) #:prefab)
(struct ast:group:arg ast:basic (type) #:prefab)
(struct ast:node ast:basic (args pattern) #:prefab)
(struct ast:node:arg ast:basic (type) #:prefab)

(define ast:pat/c
  (flat-rec-contract pat/c
                     (struct/c ast:pat:datum symbol?)
                     (struct/c ast:pat:single (maybe/c (cons/c symbol? syntax?)) (maybe/c symbol?))
                     (struct/c ast:pat:multiple (listof pat/c))
                     (struct/c ast:pat:repeat pat/c (cons/c (maybe/c natural-number/c)
                                                            (maybe/c natural-number/c)))))

(define ast:id/c (assoc/c symbol? syntax?))
(define ast:info/c (listof (cons/c symbol? (listof syntax?))))
(define ast:type/c ast:type?)
(define ast:node/c (struct/c ast:node
                             ast:id/c
                             ast:info/c
                             (listof (struct/c ast:node:arg ast:id/c ast:info/c ast:type/c))
                             ast:pat/c))
(define ast:group/c (struct/c ast:group
                              ast:id/c
                              ast:info/c
                              syntax?
                              (listof (struct/c ast:group:arg ast:id/c ast:info/c ast:type/c))
                              (assoc/c symbol? ast:node/c)))
(define ast/c (struct/c ast syntax? ast:id/c (assoc/c symbol? ast:group/c) ast:info/c))

(define (make-ast-id orig) `((0 . ,orig) ;; (t0 . ,(generate-temporary orig))
                             ))
(define (get-ast-id ast) (ast:basic-id ast))
(define (empty-ids) '())
(define (add-id key val ids) (cons (cons key val) ids))
(define (add-ids keys vals ids) (append (map cons keys vals) ids))
(define (get-id key ids) (assoc-default key ids))

(define get-struct-id (curry get-id 'rkt-struct))
(define get-oid (curry get-id '0))
(define get-fid (curry get-id 'f))
(define get-sid (curry get-id 'spec))
(define get-tid (curry get-id 't0))

(define (group-nodes gs) (map cdr (ast:group-nodes gs)))

(define (find-group-spec group-id ast-spec)
  (assoc-default (->symbol group-id) (ast-groups ast-spec)))

(define (full-group-args gs as)
  (if gs
      (append (if (ast:group-parent gs)
                  (full-group-args (find-group-spec (ast:group-parent gs) as) as)
                  '())
              (ast:group-args gs))
      '()))

(define (find-node-spec node-id group-spec/id ast-spec)
  (match group-spec/id
    [(? (or/c syntax? symbol?))
     (find-node-spec node-id (find-group-spec group-spec/id ast-spec) ast-spec)]
    [(ast:group gid ginfo prnt args nds) (assoc-default (->symbol node-id) nds)]))

(define (group-contains-node? node-spec/id group-spec)
  (define (eqn? n)
    (cond [(ast:node? node-spec/id) (equal? node-spec/id (cdr n))]
          [else (equal? (->symbol node-spec/id) (car n))]))
  (findf eqn? (ast:group-nodes group-spec)))

(define (find-node-group node-spec/id ast-spec)
  (define (contains-node ga) (group-contains-node? node-spec/id (cdr ga)))
  (cdr (findf contains-node (ast-groups ast-spec))))

(define (find-group/node-spec spec-id ast-spec)
  (define (f? ga)
    (or (and (equal? (->symbol spec-id) (car ga)) (cdr ga))
        (find-node-spec spec-id (cdr ga) ast-spec)))
  (find-first (ast-groups ast-spec) f?))
(define (group-parents group-id ast-spec)
  (if group-id
      (cons group-id
            (group-parents (ast:group-parent (find-group-spec group-id ast-spec)) ast-spec))
      '()))
(define (find-node-arg node-spec aid)
  (match-define (ast:node nids ninfo nargs npat) node-spec)
  (define (is? arg)
    (match-define (ast:node:arg aids ainfo atype) arg)
    (equal? (get-id 'wtype aids) aid))
  (findf is? nargs))

(define (format-info inf) (info-value 'format inf))
(define (format-group-id spec gspec)
  (match-define (ast tid tids grps info) spec)
  (match-define (ast:group gids ginfo gparent gargs gnodes) gspec)
  (define gprs (group-parents (get-oid gids) spec))
  (ast-format-id tid (cdr gprs) (get-oid gids) (or (format-info ginfo) (format-info info))))
(define (format-node-id spec nspec)
  (match-define (ast tid tids grps info) spec)
  (match-define (ast:node nids ninfo nargs npat) nspec)
  (match-define (ast:group gids ginfo gparent gargs gnodes) (find-node-group nspec spec))
  (define gprs (group-parents (get-oid gids) spec))
  (ast-format-id tid gprs (get-oid nids) (or (format-info ninfo) (format-info ginfo) (format-info info))))

(define (from-private aid ps formatter)
  (define (do-group gs)
    (match-define (prv:ast:group gid parent nodes ginfo^) gs)
    (define ginfo (dedup-assoc ginfo^))
    (define (do-group-args args)
      (for/list ([arg args])
        (define (f i (info '()))
          (define-values (arg-id arg-typ) (group-arg-id&type i info ginfo))
          (ast:group:arg (add-id 'f arg-id (make-ast-id i)) info arg-typ))
        (syntax-parse arg
          [i:identifier (f #`i)]
          [(i:identifier ki:keyword-info) (f #`i (attribute ki.spec))])))
    (let* ([group-args (do-group-args (assoc-default `common ginfo '()))])
      (define (do-node ns)
        (match-define (prv:ast:node nid pattern ninfo^) ns)
        (define ninfo (dedup-assoc ninfo^))
        (define (do-args pat (depth 0))
          (match pat
            [(ast:pat:single c s)
             (define arg-info '())
             (define-values (arg-id arg-typ) (node-arg-id&type c s depth ninfo))
             (define ids (add-id 'f arg-id (make-ast-id s)))
             (ast:node:arg ids arg-info arg-typ)]
            [(ast:pat:datum d) '()]
            [(ast:pat:multiple s) (for/list ([p s]) (do-args p depth))]
            [(ast:pat:repeat r k) (do-args r (cons k depth))]))
        (cons (->symbol nid)
              (ast:node (make-ast-id nid) ninfo (flatten (do-args pattern)) pattern)))
      (cons (->symbol gid)
            (ast:group (make-ast-id gid) ginfo parent group-args (map do-node nodes)))))
  (match ps
    [(prv:ast gs inf) (ast aid (make-ast-id aid) (map do-group gs) (add-info 'format formatter (dedup-assoc inf)))]))

(define (store-syntax val)
  (let store ([v val])
    (match v
      [(? symbol?) #``#,v]
      [(? syntax?) #`#'#,v]
      [(list vs ...) #`(list #,@(map store vs))]
      [(cons a b) #`(cons #,(store a) #,(store b))]
      [(ast:type:internal d of) #`(ast:type:internal #,(store d) #,(store of))]
      [(ast:type:check d chk) #`(ast:type:check #,(store d) #,(store chk))]
      [(ast:type:identifier d) #`(ast:type:identifier #,(store d))]
      [(ast:type:intrinsic d k) #`(ast:type:intrinsic #,(store d) #,(store k))]
      [(ast:type:unknown d) #`(ast:type:unknown #,(store d))]
      [(ast:format t? ts mc ms bs)
       #`(ast:format #,(store t?) #,(store ts) #,(store mc) #,(store ms) #,(store bs))]
      [(ast:group:arg id info type)
       #`(ast:group:arg #,(store id) #,(store info) #,(store type))]
      [(ast:node:arg id info type)
       #`(ast:node:arg #,(store id) #,(store info) #,(store type))]
      [(ast:pat:single c id)
       #`(ast:pat:single #,(store c) #,(store id))]
      [(ast:pat:datum syn)
       #`(ast:pat:datum #,(store syn))]
      [(ast:pat:multiple specs)
       #`(ast:pat:multiple (vector-immutable #,@(for/list ([s specs]) (store s))))]
      [(ast:pat:repeat spec k)
       #`(ast:pat:repeat #,(store spec) #,(store k))]
      [(ast:node nid ninfo nargs npat)
       #`(ast:node #,(store nid) #,(store ninfo) #,(store nargs) #,(store npat))]
      [(ast:group gid ginfo gparent gargs gnodes)
       #`(ast:group #,(store gid) #,(store ginfo) #,(store gparent) #,(store gargs) #,(store gnodes))]
      [(ast id tid groups info)
       #`(ast #,(store id) #,(store tid) #,(store groups) #,(store info))]
      [else v])))

(define (pretty-info info)
  (for/list ([ip info])
    (cons (car ip) (map syntax->datum (flatten (cdr ip))))))
(define (pretty-id id) (assoc-default 0 id))
(define (pretty-arg arg)
  (match arg
    [(ast:basic id inf) (pretty-id id)]))
(define (pretty-pattern pattern)
  (match pattern
    [(ast:pat:single #f id) `(~s ,(syntax-e id))]
    [(ast:pat:single check id) `(~s? ,(syntax-e check) ,id)]
    [(ast:pat:datum syn) `',syn]
    [(ast:pat:multiple specs)
     (map pretty-pattern (vector->list specs))]
    [(ast:pat:repeat spec k)
     `(~r ,(pretty-pattern spec) ,k)]))
(define (pretty-node node)
  (match-define (ast:node nid ninfo nargs npat) node)
  `(,(pretty-id nid)
    ,(pretty-info ninfo)
    ,@(map pretty-arg nargs)
    ,(pretty-pattern npat)))
(define (pretty-group grp)
  (match-define (ast:group gid ginfo gparent gargs gnodes) grp)
  `(,(pretty-id gid) ,(cond [(syntax? gparent) (syntax-e gparent)]
                            [(symbol? gparent) gparent]
                            [else '||])
                     ,(pretty-info ginfo)
                     ,@(map pretty-arg gargs)
                     ,@(for/list ([np gnodes])
                         `((#:node ,(car np)) ,(pretty-node (cdr np))))))

(define (pretty-spec spec)
  (match-define (ast id sid groups info) spec)
  `((#:ast ,(syntax-e id) ,(pretty-id sid))
    ,@(for/list ([gp groups])
        `((#:group  ,(car gp)) ,(pretty-group (cdr gp))))
    ,(pretty-info info)))
