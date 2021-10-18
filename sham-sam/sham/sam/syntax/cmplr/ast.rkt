#lang racket

(require racket/syntax
         syntax/parse
         (for-template racket
                       racket/stxparam
                       racket/splicing))

(require "reqs.rkt"
         "pat.rkt"
         "utils.rkt"
         "state.rkt"
         "basic.rkt"
         "syntax.rkt"
         "pat-stx.rkt"
         (for-template "runtime.rkt")
         (for-template (prefix-in rt: "../../runtime/transform.rkt")))

(provide (all-defined-out))

(define make-operator-stxid #`make)

(struct ast-pat-node-operator [ast-spec]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (ast-pat-node-operator ast-spec) op)
     ;; (printf "ast-pat-node-operator:\n ~a\n ~a\n" stx state)
     (match-define (cmplr:state:node spec-state dirs path) state)
     (match-define (cmplr:spec-state:node cspec gspec nspec) spec-state)
     (match-define (cmplr:group gid (cmplr:header:type gfrom gto) nodes info) gspec)
     (define ast-group-spec (find-group-spec gfrom ast-spec))
     (unless ast-group-spec (error 'sham/sam/transform "cannot find ast specification for ~a" gfrom))

     (define (parse-ast-node-pattern node-id node-rst-stx ast-node-spec)
       (match-define (cmplr:state:node spec-state dirs path) state)
       ;; (match-define (cmplr:spec-state:node cspec gspec nspec) spec-state)
       ;; (match-define (cmplr:group gid (cmplr:header:type gfrom gto) nodes info) gspec)

       (define (parse-seq-node-stx node-stx)
         (match-define (ast:node ids info args node-pat) ast-node-spec)
         (match-define initial-parse (stx-path-for-pattern node-pat (datum->syntax #f node-rst-stx)))
         (define initial-depth (if (ast-path? path) (ast-path-depth path) #f))
         ;; (printf "parse-seq-node-stx:initial-parse: \n~a\n" initial-parse)
         (define (recur-pattern-parse parse depth) ;; TODO keep track of ooo depth
           (match parse
             [`(seq ,args ,(pat:seq arg-pats))
              (for/fold ([arg-stxs '()]
                         [arg-dirs '()]
                         #:result (values (cmplr:pat:seq arg-stxs) arg-dirs))
                        ([arg args]
                         [p arg-pats])
                (define-values (arg-stx arg-dir) (recur-pattern-parse arg depth))
                (values (cons arg-stx arg-stxs) (append arg-dir arg-dirs)))]
             [`(var ,arg ,pat)
              (define-values (arg-stx arg-state) (frec arg (cmplr:state:node spec-state dirs (ast-path pat depth))))
              (match-define (cmplr:state:node _ arg-dirs p) arg-state)
              (values arg-stx arg-dirs)]
             [`(ooo ,args ,(pat:ooo p k))
              (for/fold ([arg-stxs '()]
                         [arg-dirs '()]
                         [ooo-depth depth]
                         #:result (values arg-stxs arg-dirs))
                        ([arg args])
                (define-values (arg-stx arg-dir) (recur-pattern-parse arg ooo-depth))
                (define new-depth
                  (match arg
                    [`(.. ,s ,p) (cons k depth)]
                    [else depth]))
                (values (cons arg-stx arg-stxs) (append arg-dirs arg-dir) new-depth))]
             [`(dat ,s ,pat) (values #f '())]
             [`(.. ,s ,pat) (values #`(... ...) '())]
             [else (error 'sham/sam/cmplr "unknown parse in pattern ~a ~a\n" parse pat)]))
         (define-values (arg-stxs arg-dirs) (recur-pattern-parse initial-parse initial-depth))
         (values (cmplr:pat:ast:node arg-stxs ast-node-spec)
                 (cmplr:state:node spec-state (append dirs arg-dirs) (ast-path #f #f))))

       (define (parse-var-node-stx)
         (values (cmplr:pat:ast:node #f ast-node-spec) state))

       ;; parse seq and just a single var pattern seperately
       (if ast-node-spec
           (if node-rst-stx
               (parse-seq-node-stx node-rst-stx)
               (parse-var-node-stx))
           (values stx state)))

     (define (try-ast stx)
       (define-values (node-id node-rst-stx)
         (match (syntax-e stx)
           [(cons fst rst) (values fst rst)]
           [id (values id #f)]))
       (cond
         [(find-node-spec node-id ast-group-spec ast-spec)
          =>
          (λ (ns) (parse-ast-node-pattern node-id node-rst-stx ns))]
         [else (values stx state)]))
     (if (syntax? stx) (try-ast stx) (values stx state)))])

(struct ast-make-body-operator [ast-spec make-op-id]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax ambo stx state frec)
     (match-define (ast-make-body-operator ast-spec make-op-id) ambo)
     (define (perform-make node&args)
       (match-define (cmplr:state:node (and spec-state (cmplr:spec-state:node cspec gspec nspec)) dirs path) state)
       (match-define (cmplr (cmplr:header cid cargs (cmplr:header:type cfrom cto)) groups info) cspec)
       (match-define (cmplr:group gid (cmplr:header:type gfrom gto) nodes ginfo) gspec)
       (match-define (cons stxid node-args) node&args)
       (define node-spec
         (match (split-identifier stxid)
           [(list #f node-id) (find-node-spec node-id gto ast-spec)]
           [(list ': group-id node-id) (find-node-spec node-id group-id ast-spec)]
           [else (error 'sham/sam/transform "unknown type of node stxid ~a" stxid)]))
       (unless (ast:node? node-spec)
         (error 'sham/sam/transform "unknown node for make operator ~a ~a" stxid node-spec))
       (define node-fid (get-fid (ast:basic-id node-spec)))
       (define node-make-id (format-id node-fid "make-~a" node-fid))
       (define-values (node-args-stxs new-state) (mapl/state frec state node-args))
       ;; TODO add metadata
       (values #`(#,node-make-id #,@node-args-stxs) new-state))
     (define (make-op-args stx)
       (match (syntax-e stx)
         [(list op-id node-id node-args ...)
          #:when (equal? (->symbol op-id) (->symbol make-op-id))
          (cons node-id node-args)]
         [else #f]))
     (cond
       [(and (syntax? stx) (make-op-args stx)) => perform-make]
       [else (values stx state)]))])

(define (compile-ast-type var pat depth app-info ast-spec cstate)
  ;; (printf "compile-ast-type: ~a ~a ~a ~a\n" var pat depth app-info)
  (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) dirs path) cstate)
  (match-define (cmplr header groups info) cspec)
  (match-define (cmplr:header cid cargs ctyp) header)
  (define arg-stxs
    (for/list ([arg (map car cargs)])
      (cond [(info-value (syntax->datum arg) app-info) => car]
            [else arg])))
  (define (compile-group-at-depth cmplr-group depth)
    (match-define (cmplr:group id gtype nodes ginfo) cmplr-group)
    (let rec ([depth depth]
              [val var])
      (match depth
        [#f #`(#,id #,val #,@arg-stxs)]
        [`((,mn . ,mx) . ,rst)
         (define nval (generate-temporary var))
         #`(map (λ (#,nval) #,(rec rst nval)) #,val)]
        [else (error 'sham/sam/TODO "compile deeper ~a ~a ~a" var depth id)])))
  (define (compile-spec-type depth spec)
    (match spec
      [(ast:group gids info parent args nodes)
       (define group-fid (get-fid gids))
       (define group-oid (get-oid gids))
       (define (is-group? g)
         (match-define (cmplr:group id gtype nodes ginfo) g)
         (match-define (cmplr:header:type from to) gtype)
         (or (equal? (->symbol from) (->symbol group-fid))
             (equal? (->symbol from) (->symbol group-oid)) ))
       (define cmplr-group (find-first groups is-group?))
       (unless cmplr-group
         (error 'sham/sam/cmplr "no compiler group for ~a" group-fid))
       (compile-group-at-depth cmplr-group depth)]
      [else (error 'sham/sam/cmplr "not a group ast spec for compiling ~a ~a" var spec)]))
  (match (get-ast-type pat depth ast-spec)
    [(ast:type:internal depth spec) (compile-spec-type depth spec)]
    [(ast:type:intrinsic depth t) (error 'sham/sam/TODO "intrinsic ast type ~a ~a" t ast-spec)]
    [(ast:type:identifier depth) (let rec ([d depth] [v var])
                                   (match d
                                     [#f #`(rt:identifier->syntax #,v)]
                                     [`((,mn . ,mx) . ,rst)
                                      (define nval (generate-temporary var))
                                      #`(map (λ (#,nval) #,(rec rst nval)) #,v)]))]
    [t (error 'sham/sam/TODO "compile-ast-type ~a ~a ~a" var t ast-spec)]))

(struct ast-pat-compile-var-operator [op-stxid ast-spec]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (ast-pat-compile-var-operator op-stxid ast-spec) op)
     ;; (printf "ast-pat-compile-var-operator:\n ~a\n ~a\n" stx state)
     (define (do-pat stx)
       (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) dirs path) state)
       (syntax-parse stx
         [(op res-id info:keyword-info)
          #:when (free-identifier=? #`op op-stxid)
          (define gen-id (format-id #`res-id "v-~a" #`res-id))
          (match-define (ast-path pat depth) path)
          (define var-pat (cmplr:pat:tvar gen-id #`res-id pat))
          (define compile-dir
            (cmplr:dir:bind (cmplr-bind-var #`res-id depth)
                            (compile-ast-type gen-id pat depth (attribute info.spec) ast-spec state)))
          (values var-pat (append-dir-in-state compile-dir state))]
         [else (values stx state)]))
     (if (syntax? stx) (do-pat stx) (values stx state)))])

(struct ast-default-rec-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (if (syntax? stx)
         (match (syntax-e stx)
           [(list ss ...) (mapl/state frec state ss)]
           [i (values stx state)])
         (values stx state)))])

(struct ast-normal-var-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (if (identifier? stx)
         (match-let ([(cmplr:state:node spec dirs (ast-path pat depth)) state]
                     [gen-stxid (generate-temporary stx)])
           (values (cmplr:pat:tvar gen-stxid stx pat)
                   (cmplr:state:node spec
                                     (append dirs
                                             (list (cmplr:dir:bind (cmplr-bind-var stx depth) gen-stxid)))
                                     (ast-path #f #f))))
         (values stx state)))])

(define (ast-node-pat-builder ctype pat-stx node-state)
  (match-define (cmplr-ast-source ast-spec) ctype)
  (match-define (cmplr:state:node (and spec-state (cmplr:spec-state:node cspec gspec nspec)) orig-dirs path) node-state)
  (match-define (cmplr (cmplr:header cid cargs (cmplr:header:type cfrom cto)) groups info) cspec)
  (define-values (new-pat-stx pat-state) (basic-stx-rec pat-stx (cmplr:state:node spec-state '() path) (info-value ik-node-pat-ops info)))
  (match-define (cmplr:state:node pat-spec pat-dirs pat-path) pat-state)
  (values new-pat-stx (cmplr:state:node pat-spec (append orig-dirs pat-dirs) pat-path)))

;;  TODO get group spec once at the start of pat and body builder
;; (struct cmplr:state:ast-node cmplr:state:node [group-spec ast-spec])

(define (ast-node-body-builder ctype body-stx node-state)
  (match-define (cmplr-ast-target ast-spec) ctype)
  (match-define (cmplr:state:node (and spec-state (cmplr:spec-state:node cspec gspec nspec)) dirs path) node-state)
  (match-define (cmplr (cmplr:header cid cargs (cmplr:header:type cfrom cto)) groups info) cspec)
  (basic-stx-rec body-stx (cmplr:state:node spec-state dirs (ast-path #f #f)) (info-value ik-node-body-ops info '())))

(define (update-stx-with-dirs dirs body-stx)
  (define with-dirs (filter cmplr:dir:stx:with? dirs))
  (define other-dirs (filter-not cmplr:dir:stx:with? dirs))
  (define (from-with d)
    (match-define (cmplr:dir:stx:with (stx-cls-with-var id depth) val-stx) d)
    (stx:def id val-stx))
  (define with-defs (map from-with with-dirs))
  (values other-dirs (stx:local-def #'let with-defs body-stx)))

(struct cmplr-ast-type cmplr:type [of]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cat curr-spec)
     (match-define (cmplr-ast-type of-ast) cat)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args (cmplr:header:type src tgt)) header)
     (define actual-builder
       (if (eqv? cat src)
           (cmplr-ast-source of-ast)
           (cmplr-ast-target of-ast)))
     (define new-info
       (update-info ik-spec-bs (λ (bs) (append bs (list actual-builder))) info))
     (cmplr header groups new-info))])

(struct cmplr-ast-source [ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cas curr-spec)
     (match-define (cmplr-ast-source ast-spec) cas)
     (match-define (cmplr header groups info) curr-spec)
     (define pat-ops
       (list (ast-default-rec-operator)
             (ast-normal-var-operator)
             (ast-pat-node-operator ast-spec)
             (ast-pat-compile-var-operator #`rt:^ ast-spec)))
     (define this-info
       (kw-info (ik-node-pat-bs cas)
                (ik-node-pat-ops . pat-ops)
                (ik-group-bs (cmplr-ast-group-builder ast-spec))
                (ik-top-bs (cmplr-ast-top-builder ast-spec))))
     (define new-info (combine-info info this-info))
     (cmplr header groups new-info))]
  #:methods gen:cmplr-node-pat-builder
  [(define build-node-pattern-stx ast-node-pat-builder)])

(struct cmplr-ast-target [ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cat curr-spec)
     (match-define (cmplr-ast-target ast-spec) cat)
     (match-define (cmplr header groups info) curr-spec)
     ;; (printf "cmplr-ast-target:\n")
     (define this-info
       (kw-info (ik-node-body-bs cat)
                (ik-node-body-ops (ast-make-body-operator ast-spec make-operator-stxid))
                (ik-node-bs (cmplr-ast-node-builder))))
     (define new-info (combine-info info this-info))
     (cmplr header groups new-info))]
  #:methods gen:cmplr-node-body-builder
  [(define build-node-body-stx ast-node-body-builder)])

(struct cmplr-ast-node-builder []
  #:methods gen:cmplr-node-builder
  [(define (build-node-stx canb node node-spec-state)
     ;; (printf "cmplr-ast-node-builder: ~a\n" node)
     (match-define (cmplr:node pat-stx dirs body-stx) node)
     (match-define (cmplr:spec-state:node cspec gspec nspec) node-spec-state)
     (match-define (cmplr (cmplr:header id args (cmplr:header:type cfrom cto)) groups info) cspec)
     (define-values (new-dirs new-body-stx) (update-stx-with-dirs dirs body-stx))
     (cmplr:node pat-stx new-dirs new-body-stx))])

(struct cmplr-ast-group-builder [ast-spec]
  #:methods gen:cmplr-group-builder
  [(define (build-group-stx agb node-stxs cspec gspec)
     (match-define (cmplr-ast-group-builder ast-spec) agb)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:header:type cfrom cto) cmplr-type)
     (match-define (cmplr:group gid (cmplr:header:type gfrom gto) gnodes ginfo) gspec)
     (define ast-group-spec (find-group-spec gfrom ast-spec))
     (define (to-match-pat cnode)
       (match-define (cmplr:node pat-stx dirs body-stx) cnode)
       (cmplr:ast:match:pat pat-stx #f (combine-binds-with-let dirs body-stx)))
     (define (to-child-pat gs)
       (match-define (ast:group gids info parent args nodes) gs)
       (define gfid (get-fid gids))
       (define goid (get-oid gids))
       (define (is-cmplr-group? g)
         (match-define (cmplr:group _ (cmplr:header:type cgfrom cgto) _ _) g)
         (equal? (->symbol goid) (->symbol cgfrom)))
       (define cmplr-group (find-first groups is-cmplr-group?))
       #`((? #,(format-id gfid "~a?" gfid))
          (#,(cmplr:group-id cmplr-group)
           #,cmplr-input-stxid
           #,@(map car cmplr-args))))
     (define child-delegates (map to-child-pat (find-group-children ast-group-spec ast-spec)))
     (cmplr:ast:group gid
                      cmplr-input-stxid
                      (map car cmplr-args)
                      (append (map to-match-pat node-stxs) child-delegates)))])

(struct cmplr-ast-top-builder [ast-spec]
  #:methods gen:cmplr-top-builder
  [(define (build-cmplr-top atb group-stxs cspec)
     (match-define (cmplr-ast-top-builder ast-spec) atb)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:header:type cfrom cto) cmplr-type)
     (list
      #`(define (#,cmplr-id #,cmplr-input-stxid #,@(map list (map car cmplr-args) (map cdr cmplr-args)))
          (splicing-syntax-parameterize ([this-ast (make-rename-transformer #'#,cmplr-input-stxid)])
            #,@(map to-syntax group-stxs))
          (cond
            #,@(for/list ([group groups])
                 (match-define (cmplr:group gid (cmplr:header:type gfrom gto) nodes info) group)
                 (define ast-group-spec (find-group-spec gfrom ast-spec))
                 (define gfid (get-fid (ast:basic-id ast-group-spec)))
                 (define group-check? (format-id gfid "~a?" gfid))
                 #`((#,group-check? #,cmplr-input-stxid) (#,gid #,cmplr-input-stxid #,@(map car cmplr-args))))
            [else (error "invalid input to transform ~a ~a" (quote #,cmplr-id) #,cmplr-input-stxid)]))))])
