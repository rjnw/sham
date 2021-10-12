#lang racket

(require racket/syntax
         syntax/parse)

(require "format.rkt"
         "pat.rkt"
         "kw-info.rkt"
         "utils.rkt")

(provide (all-defined-out))

(struct spec [] #:prefab)

(define (lookup-spec syn)
  (define-values (spec-value rt) (syntax-local-value/immediate syn #f))
  (or (and (spec? spec-value) spec-value)
      (and rt (lookup-spec rt))))

(module* ast #f
  (provide (all-defined-out))
  (struct ast spec (rename-id id groups info)
    #:property prop:rename-transformer 1)

  (define (lookup-ast-spec syn)
    (define spec-value (lookup-spec syn))
    (unless (ast? spec-value) (error 'sham:sam "unknown ast specification ~a:~a" syn spec-value))

    spec-value)
  (struct ast:basic (id info) #:prefab)
  (struct ast:group ast:basic (parent args nodes) #:prefab)
  (struct ast:group:arg ast:basic (type) #:prefab) ;; TODO deprecate type, instead calculate when needed using pattern
  (struct ast:node ast:basic (args pattern) #:prefab)
  (struct ast:node:arg ast:basic (type) #:prefab)

  (struct ast:pat pat () #:prefab)
  (struct ast:pat:datum pat:dat () #:prefab)
  (struct ast:pat:single pat:var (maybe-check) #:prefab)
  (struct ast:pat:multiple pat:seq () #:prefab)
  (struct ast:pat:repeat pat:ooo () #:prefab)

  (struct ast:type (depth) #:prefab)
  (struct ast:type:internal ast:type (of) #:prefab)
  (struct ast:type:check ast:type (chk) #:prefab)
  (struct ast:type:identifier ast:type () #:prefab)
  (struct ast:type:intrinsic ast:type (kind) #:prefab)
  (struct ast:type:unknown ast:type () #:prefab)

  (define ast:pat/c
    (flat-rec-contract pat/c
                       (struct/c ast:pat:datum symbol?)
                       (struct/c ast:pat:single (maybe/c symbol?) (maybe/c (cons/c symbol? syntax?)))
                       (struct/c ast:pat:multiple (listof pat/c))
                       (struct/c ast:pat:repeat pat/c (cons/c (maybe/c natural-number/c)
                                                              (maybe/c natural-number/c)))))

  (define ast:ids/c (assoc/c symbol? syntax?))
  (define ast:info/c (listof (cons/c symbol? (listof syntax?))))
  (define ast:type/c ast:type?)
  (define ast:node/c (struct/c ast:node
                               ast:ids/c
                               ast:info/c
                               (listof (struct/c ast:node:arg ast:ids/c ast:info/c ast:type/c))
                               ast:pat/c))
  (define ast:group/c (struct/c ast:group
                                ast:ids/c
                                ast:info/c
                                syntax?
                                (listof (struct/c ast:group:arg ast:ids/c ast:info/c ast:type/c))
                                (assoc/c symbol? ast:node/c)))
  (define ast/c (struct/c ast syntax? ast:ids/c (assoc/c symbol? ast:group/c) ast:info/c))

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
    (define idsym (->symbol group-id))
    (define (is-group? grp)
      (match-define (ast:group gid ginfo prnt args nds) (cdr grp))
      (define aliases (info-1value 'alias ginfo))
      ;; (printf "find-group: ~a\n" (list group-id gid aliases grp))
      (or (equal? idsym (car grp))
          (and (get-fid gid) (equal? idsym (->symbol (get-fid gid))))
          (and (get-fid gid) (equal? idsym (->symbol (get-oid gid))))
          (and aliases
               (member idsym (map ->symbol aliases)))))
    (define grp-pair (find-first (ast-groups ast-spec) is-group?))
    ;; (assoc-default (->symbol group-id) (ast-groups ast-spec))
    (and grp-pair (cdr grp-pair)))

  (define (find-group-children group-spec ast-spec)
    (match-define (ast:group gids ginfo parent args nodes) group-spec)
    (define gid (get-oid gids))
    (define (is-child? group)
      (and (ast:group-parent (cdr group))
           (equal? (->symbol (ast:group-parent (cdr group))) (->symbol gid))))
    (map cdr (filter is-child? (ast-groups ast-spec))))

  (define (full-group-args gs as)
    (if gs
        (append (if (ast:group-parent gs)
                    (full-group-args (find-group-spec (ast:group-parent gs) as) as)
                    '())
                (ast:group-args gs))
        '()))

  (define (matches-node-spec? node-id node-spec (check-alias? #t))
    (match-define (ast:node ids info args pat) node-spec)
    (and node-spec
         (or (equal? (->symbol (get-oid ids)) (->symbol node-id))
             (and (identifier? node-id) (free-identifier=? node-id (get-fid ids)))
             (member (->symbol node-id) (map ->symbol (or (info-value 'alias info) '()))))))

  (define (find-node-spec node-id group-spec/id ast-spec (check-alias? #t))
    (match group-spec/id
      [(? (or/c syntax? symbol?))
       (find-node-spec node-id (find-group-spec group-spec/id ast-spec) ast-spec check-alias?)]
      [(ast:group gid ginfo prnt args nds)
       (find-first (map cdr nds) (λ (n) (matches-node-spec? node-id n check-alias?)))]
      [#f (locate-first (map cdr (ast-groups ast-spec)) (λ (g) (find-node-spec node-id g ast-spec check-alias?)))]))

  (define (group-contains-node? node-spec/id group-spec)
    (define (eqn? n)
      (cond [(ast:node? node-spec/id) (equal? node-spec/id (cdr n))]
            [else (equal? (->symbol node-spec/id) (car n))]))
    (findf eqn? (ast:group-nodes group-spec)))

  (define (find-node-group node-spec/id ast-spec)
    (define (contains-node ga) (group-contains-node? node-spec/id (cdr ga)))
    (cond
      [(findf contains-node (ast-groups ast-spec)) => cdr]
      [else #f]))

  (define (find-group/node-spec spec-id ast-spec)
    (define (f? ga)
      (or (and (equal? (->symbol spec-id) (car ga))
               (cdr ga))
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
      (free-identifier=? (get-oid aids) aid))
    (findf is? nargs))

  (define (format-info inf) (info-1value 'format inf))
  (define (format-group-id spec gspec)
    (match-define (ast tid tids grps info) spec)
    (match-define (ast:group gids ginfo gparent gargs gnodes) gspec)
    (define gprs (group-parents (get-oid gids) spec))
    (ast-format-id tid (reverse (cdr gprs)) (get-oid gids) (or (format-info ginfo) (format-info info))))
  (define (format-node-id spec nspec)
    (match-define (ast tid tids grps info) spec)
    (match-define (ast:node nids ninfo nargs npat) nspec)
    (match-define (ast:group gids ginfo gparent gargs gnodes) (find-node-group nspec spec))
    (define gprs (group-parents (get-oid gids) spec))
    (ast-format-id tid (reverse gprs) (get-oid nids) (or (format-info ninfo) (format-info ginfo) (format-info info))))

  (define (find-in-spec stxid curr-spec spec)
    (match curr-spec
      [(? ast?) (find-group-spec stxid curr-spec)]
      [(? ast:group?) (find-node-spec stxid curr-spec spec)]
      [(? ast:node?) #f]))

  (define (ast-from-path path curr-spec spec)
    (match path
      [(list s) (find-in-spec s curr-spec spec)]
      [(cons fst rst) (ast-from-path rst (find-in-spec fst curr-spec spec) spec)]))

  (define (ast-type-from-var-stx stxid depth spec)
    ;; (printf "ast-type-from-var-stx: ~a ~a\n" stxid (split-identifier stxid))
    (define (from-specified id typs)
      (cond [(or (ast-from-path typs spec spec)
                 (and (empty? typs) (ast-from-path (list id) spec spec)))
             => (λ (s) (ast:type:internal depth s))]
            [else (match (map syntax->datum typs)
                    [(list (or 'id 'identifier)) (ast:type:identifier depth)]
                    [(list (? intrinsic-type? t)) (ast:type:intrinsic depth t)])]))
    (match (split-identifier stxid)
      [`(! ,intr) (ast:type:intrinsic depth intr)]
      [(list (or ': '_) id typs ...) (from-specified id typs)]
      [(list #f id) (from-specified id (list id))]
      [else #f]))

  (define (get-ast-type pat depth spec)
    (match pat
      [(ast:pat:single var maybe-check)
       (or (ast-type-from-var-stx var depth spec)
           (and maybe-check (ast:type:check depth maybe-check)))]))

  (define (intrinsic-type? t) (member t `(str string integer int bool boolean sym symbol)))
  (define (id&type raw-id (c #f) (depth #f) (maybe-type #f))
    (define (from-specified k typs)
      (match* (k typs)
        [(k (list g ns ...))
         (match (syntax->datum g)
           [(or 'id 'identifier) (ast:type:identifier depth)]
           [(? intrinsic-type? t) (ast:type:intrinsic depth t)]
           [else (ast:type:internal depth typs)])]))
    (cond
      [c (values raw-id (ast:type:check c depth))]
      [else
       (match (split-identifier raw-id)
         [`(! ,intr) (values intr (ast:type:intrinsic intr depth))]
         [`(#f ,s) (values s (if maybe-type maybe-type (ast:type:unknown depth)))] ;; check type for specific id
         [`(,k ,id ,typs ...) (values id (from-specified k typs))]
         [else (values raw-id (ast:type:unknown depth))])]))

  (define (node-arg-id&type c s depth info)
    (id&type s c depth (info-value 'type info)))
  (define (group-arg-id&type id ainfo ginfo)
    (id&type id #f #f (or (info-value 'type ainfo) (info-value 'common-type ginfo))))

  (define (build-ast-spec raw-spec formatter)
    (define (do-group gs)
      (match-define (ast:group gid ginfo^ parent #f nodes) gs)
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
          (match-define (ast:node nid ninfo^ #f pattern) ns)
          (define ninfo (dedup-assoc ninfo^))
          (define (do-args pat (depth #f))
            (match pat
              [(ast:pat:single s c)
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
    (match raw-spec
      [(ast #f aid gs inf)
       (ast aid (make-ast-id aid) (map do-group gs) (add-info 'format formatter (dedup-assoc inf)))]))

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
        [(ast:pat:single id c)
         #`(ast:pat:single #,(store id) #,(store c))]
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
      (cons (car ip) (map (λ (s) (if (syntax? s) (syntax->datum s) s)) (flatten (cdr ip))))))
  (define (pretty-id id) (assoc-default 0 id))
  (define (pretty-arg arg)
    (match arg
      [(ast:basic id inf) (pretty-id id)]))
  (define (pretty-pattern pattern)
    (match pattern
      [(ast:pat:single id #f) `(~s ,(syntax-e id))]
      [(ast:pat:single id check) `(~s? ,(syntax-e check) ,id)]
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
      ,(pretty-info info))))

(module* compiler #f
  (provide (all-defined-out))
  (struct cmplr spec [header groups info] #:prefab)
  (struct cmplr:header [id args type] #:prefab)
  (struct cmplr:header:type [from to] #:prefab)
  (struct cmplr:group [id type nodes info] #:prefab)
  (struct cmplr:node [pat dirs body] #:prefab)

  (struct cmplr:spec-state:node [cspec gspec nspec])
  (struct cmplr:state:node [spec bvars&dirs path] #:prefab)
  (struct cmplr:type spec [])
  )

(module* reader #f
  (provide (all-defined-out))
  (struct reader spec [readf-ids ast-id info] #:prefab))
