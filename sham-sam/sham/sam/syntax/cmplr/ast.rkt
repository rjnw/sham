#lang racket

(require racket/syntax
         (for-template racket))

(require "reqs.rkt"
         "pat.rkt"
         "utils.rkt"
         "state.rkt"
         "basic.rkt"
         "syntax.rkt"
         "pat-stx.rkt"
         (for-template (prefix-in rt: "../../runtime/transform.rkt")))

(provide (all-defined-out))

;; (define (ast-match-pattern stx ast-spec maybe-group-spec)
;;   (define node-spec
;;     (match (syntax-e stx)
;;       [(cons fst rst) (find-node-spec fst maybe-group-spec ast-spec)]
;;       [id (find-node-spec stx maybe-group-spec ast-spec)]))
;;   (define (parse-as-node node-spec stx)
;;     (match-define (ast:node ids info args pat) node-spec)
;;     (stx-with-pat->match-pattern stx pat))
;;   (and node-spec (parse-as-node node-spec stx)))



(struct ast-pat-node-operator [ast-spec]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (ast-pat-node-operator ast-spec) op)
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
         (match-define initial-parse (stx-path-for-pattern node-pat node-rst-stx))
         (define (recur-pattern-parse parse)
           (match parse
             [`(seq ,args ,(pat:seq arg-pats))
              (for/fold ([arg-stxs '()]
                         [arg-dirs '()]
                         #:result (values (cmplr:pat:seq arg-stxs) arg-dirs))
                        ([arg args]
                         [p arg-pats])
                (define-values (arg-stx arg-dir) (recur-pattern-parse arg p))
                (values (cons arg-stx arg-stxs) (append arg-dir arg-dirs)))]
             [`(var ,arg ,pat)
              (define-values (arg-stx arg-state) (frec arg (cmplr:state:node spec-state dirs pat)))
              (match-define (cmplr:state:node _ arg-dirs p) arg-state)
              (values arg-stx arg-dirs)]
             [`(ooo ,args ,pat)
              (for/fold ([arg-stxs '()]
                         [arg-dirs '()])
                        ([arg args])
                (define-values (arg-stx arg-dir) (recur-pattern-parse arg pat))
                (values (cons arg-stx arg-stxs) (append arg-dir arg-dirs)))]
             [`(dat ,s ,pat) (values #`'#,s '())]
             [`(.. ,s ,pat) (values #`(... ...) '())]
             [else (error 'sham/sam/cmplr "unknown parse in pattern ~a ~a\n" parse pat)]))
         (define-values (arg-stxs arg-dirs) (recur-pattern-parse initial-parse))
         (values (cmplr:pat:ast:node arg-stxs ast-node-spec)
                 (cmplr:state:node spec-state (append dirs arg-dirs) #f)))

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

(define (compile-ast-type var ast-type cstate)
  (printf "compile-ast-type: ~a ~a\n" var ast-type)
  (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) dirs path) cstate)
  (match-define (cmplr header groups info) cspec)
  (match-define (cmplr:header cid cargs ctyp) header)
  (error 'sham/sam/TODO "compile-ast-type ~a ~a" var ast-type)

  ;; (define (perform-compile cmplr-group depth)
  ;;   (match-define (cmplr:group id gtype nodes ginfo) cmplr-group)
  ;;   (let rec ([depth depth]
  ;;             [val var])
  ;;     (match depth
  ;;       [#f #`(#,id #,val #,@extra-cargs)]
  ;;       [`((,mn . ,mx) . ,rst)
  ;;        (define nval (generate-temporary var))
  ;;        #`(map (λ (#,nval) #,(rec rst nval)) #,val)
  ;;        ;; #`(map (curryr #,id #,@extra-cargs) #,val)
  ;;        ]
  ;;       [else (error 'sham/sam/TODO "compile deeper ~a ~a ~a" var depth id)])))
  ;; (match ast-type
  ;;   [(ast:type:internal depth (list group top))
  ;;    (define group-fid (get-fid (ast:basic-id group)))
  ;;    (define group-oid (get-oid (ast:basic-id group)))
  ;;    (define (is-group? g)
  ;;      (match-define (cmplr:group id gtype nodes ginfo) g)
  ;;      (match-define (cmplr:type from to) gtype)
  ;;      (or (equal? (->symbol from) (->symbol group-fid))
  ;;          (equal? (->symbol from) (->symbol group-oid)) ))
  ;;    (define ast-group (find-first groups is-group?))
  ;;    (unless ast-group
  ;;      (error 'sham/sam/cmplr "no compiler group for ~a" group-fid))
  ;;    (perform-compile ast-group depth)]
  ;;   [(ast:type:identifier depth)
  ;;    #`(rt:identifier->syntax #,var #'#,var)])
  )

(struct ast-pat-compile-var-operator [op-stxid]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (ast-pat-compile-var-operator op-stxid) op)
     (printf "ast-pat-compile-var-operator: ~a ~a\n" stx state)
     (define (do-pat ss)
       (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) dirs path) state)
       (match ss
         [(list op res-id args ...)
          #:when (free-identifier=? op op-stxid)
          (define gen-id (format-id res-id "v-~a" res-id))
          (define var-pat (cmplr:pat:tvar gen-id res-id path))
          (define compile-dir (cmplr:dir:bind gen-id (compile-ast-type res-id path state)))
          (values var-pat (append-dir-in-state compile-dir state))]
         [else (values stx state)]))
     (if (syntax? stx) (do-pat (syntax-e stx)) (values stx state)))])

(struct ast-pat-normal-var-operator [stxid binder]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (printf "ast-pat-normal-var-operator: ~a ~a\n" stx state)
     ;; (body-operator-gen-syntax bvp rst-stx frec cstate)
     ;; (match-define (bind-var-pattern id binder) bvp)
     ;; (match-define (cmplr:state:node cspec gspec nspec bvars) cstate)
     ;; (match-define (cmplr header groups info) cspec)
     ;; (syntax-parse rst-stx
     ;;   [([vars vals] ... bodys ...)
     ;;    (let*-values ([(vars-stx vars-state) (mapr/state frec cstate (attribute vars))]
     ;;                  [(vals-stx vals-state) (mapr/state frec vars-state (attribute vals))]
     ;;                  [(bodys-stx bodys-state) (mapr/state frec vals-state (attribute bodys))])
     ;;      (values (binder (map stx:def vars-stx vals-stx) bodys-stx) bodys-state))])
     )])

(struct ast-default-rec-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match (syntax-e stx)
       [(list ss ...) (mapl/state frec state ss)]
       [i (values stx state)]))])

(define (ast-node-pat-builder ctype pat-stx node-state)
  (match-define (cmplr-ast-source ast-spec) ctype)
  (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) '() path) node-state)
  (match-define (cmplr (cmplr:header cid cargs (cmplr:header:type cfrom cto)) groups info) cspec)
  (basic-stx-rec pat-stx node-state (info-value ik-node-pat-ops info))

  ;; (match-define (cmplr:state:node cspec gspec nspec bvars) node-state)
  ;; (match-define (cmplr header groups info) cspec)
  ;; (match-define (cmplr:header cid cargs ctyp) header)
  ;; (match-define (cmplr:type from-spec to-spec) ctyp)
  ;; (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
  ;; (match-define (cmplr:type gfrom gto) gtyp)
  ;; (match-define (cmplr:node pats bodys) nspec)

  ;; (define all-operators (flatten (info-values 'operators info)))
  ;; (define bind-operators (filter cmplr-bind-operator? all-operators))
  ;; (define find-bind-operator (find-operator bind-operators bind-operator-identifier))

  ;; (define ((cmplr-pattern stx ast-type) op)
  ;;   (let ([op-pat (bind-operator-parse-syntax op stx ast-type)])
  ;;     (if (pat:var? op-pat)
  ;;         (values (list (cons op-pat op)) op-pat)
  ;;         (values '() op-pat))))

  ;; -> (values (listof bound-vars) stx)
  ;; (define ((ast-pattern rst) ast-node-spec)
  ;;   (match-define (ast:node nid ninfo nargs npat) ast-node-spec)
  ;;   ;; (printf "ast-pattern:\n")
  ;;   ;; (pretty-print (pretty-node ast-node-spec))
  ;;   ;; (printf "~a\n\n" rst)

  ;;   (define (from-ast-type ast-type)
  ;;     (match ast-type
  ;;       [(ast:type:internal depth (cons grp subs))
  ;;        (define ast-group (find-group-spec grp from-spec))
  ;;        (if ast-group
  ;;            (ast:type:internal depth (list ast-group from-spec))
  ;;            (error 'sham/sam "couldn't find ast type: ~a" grp))]
  ;;       [(ast:type depth) ast-type]))
  ;;   (define-values (node-vars node-stx)
  ;;     (let rec ([parse (stx-path-for-pattern npat #`(#,@rst))])
  ;;       ;; (printf "rec: ~a\n" parse)
  ;;       ;; (stx-path-for-pattern npat #`(#,@rst))

  ;;       (match parse
  ;;         [`(seq ,subs ,(pat:seq (vector ps ...)))
  ;;          (define-values (vars stxs) (fold-with-vars rec subs))
  ;;          (values vars (cmplr:pat:seq stxs))]
  ;;         [`(var ,stx ,var-pat)
  ;;          (match-define (ast:pat:single id check) var-pat)
  ;;          (define node-arg (find-node-arg ast-node-spec id))
  ;;          (do-pattern stx (from-ast-type (ast:node:arg-type node-arg)))]
  ;;         [`(ooo ,subs ,pat)
  ;;          ;; (printf "ooo: ~a\n" subs)
  ;;          (fold-with-vars rec subs)]
  ;;         [`(___ ,s ,nx ,pat) (values '() s)]
  ;;         [`(dat #f ,_) (values '() #f)])))
  ;;   (values node-vars (cmplr:pat:ast-node ast-node-spec (flatten node-stx))))

  ;; -> (values bind-vars match-pat)
  ;; (define (do-pattern stx maybe-ast-spec)
  ;;   ;; (printf "bind-pattern: ~a\n" stx)
  ;;   (match (syntax-e stx)
  ;;     [(? symbol?) (cond
  ;;                    [(find-bind-operator #'#%var) => (cmplr-pattern stx maybe-ast-spec)]
  ;;                    [else (values '() (cmplr:pat:var stx (generate-temporary stx)))])]
  ;;     [(cons fst rst)
  ;;      (cond
  ;;        [(find-node-spec fst from-group-ast-spec from-spec) => (ast-pattern rst)]
  ;;        [(or (find-bind-operator fst) (find-bind-operator #'#%app)) => (cmplr-pattern stx maybe-ast-spec)]
  ;;        ;; [(find-bind-operator #'#%app) => (cmplr-pattern stx maybe-ast-spec)]
  ;;        [else
  ;;         ;; keep the pattern as is in fst but recur over rst
  ;;         (define-values (vars stxs) (fold-with-vars (curryr do-pattern maybe-ast-spec) rst))
  ;;         (values vars (stx:app fst stxs))]
  ;;        ;; [else (error 'sham/sam "unknown sequence in bind pattern ~a ~a" fst rst)]
  ;;        )]))

  ;; (define from-group-ast-spec
  ;;   (cond
  ;;     [(find-group/node-spec gfrom from-spec) => cdr]
  ;;     [else (error 'sham/sam "couldn't locate ast group for compiling: ~a" gfrom)]))

  ;; (printf "\n\nparsing-node: ~a\n" bind)
  ;; (define-values (pat-vars pat-stx) (do-pattern pat-spec from-group-ast-spec))
  ;; (values pat-stx (cmplr:state:node cspec gspec nspec pat-vars))
  )

(define (ast-node-body-builder ctype body-stx node-state)
  (match-define (cmplr-ast-target ast-spec) ctype)
  (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) dirs path) node-state)
  (match-define (cmplr (cmplr:header cid cargs (cmplr:header:type cfrom cto)) groups info) cspec)
  (basic-stx-rec body-stx node-state (info-value ik-node-body-ops info))

  ;; (match-define (cmplr:state:node cspec gspec nspec bvars) node-state)
  ;; (match-define (cmplr header groups info) cspec)
  ;; (match-define (cmplr:header cid cargs ctyp) header)
  ;; (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
  ;; (match-define (cmplr:type from-spec to-spec) gtyp)
  ;; (match-define (cmplr:node pats bodys) nspec)

  ;; (define all-operators (flatten (info-values 'operators info)))
  ;; (define body-operators (filter cmplr-body-operator? all-operators))
  ;; (define find-body-operator (find-operator body-operators body-operator-identifier))

  ;; (define (bvars&body bvars raw-body-stxs)
  ;;   (define-values (bvar-stxs bvar-state)
  ;;     (mapr/state (λ (bv state) (bind-operator-gen-syntax (cdr bv) (car bv) state)) node-state bvars))

  ;;   (define (do-body body-stx body-state)
  ;;     (match (syntax-e body-stx)
  ;;       [(cons fst rst)
  ;;        (cond [(or (find-body-operator fst) (find-body-operator #'#%app))
  ;;               => (λ (op) (body-operator-gen-syntax op body-stx do-body body-state))]
  ;;              [else
  ;;               (define-values (rst-stxs nstate) (mapr/state do-body body-state rst))
  ;;               (values (datum->syntax body-stx (cons fst rst-stxs))
  ;;                       nstate)])]
  ;;       [else (values body-stx body-state)]))

  ;;   (define-values (body-stxs body-state) (mapr/state do-body bvar-state raw-body-stxs))
  ;;   (append bvar-stxs body-stxs))
  ;; (bvars&body bvars body-spec)
  )

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
     ;; (printf "\tnew-info: ~a\n" new-info)
     (cmplr header groups new-info))])

(struct cmplr-ast-source [ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cas curr-spec)
     (match-define (cmplr-ast-source ast-spec) cas)
     (match-define (cmplr header groups info) curr-spec)
     (printf "cmplr-ast-source:\n")
     (define pat-ops
       (list (ast-default-rec-operator)
             (ast-pat-node-operator ast-spec)
             (ast-pat-compile-var-operator #`rt:^)))
     (define this-info
       (kw-info (ik-node-pat-bs cas)
                (ik-node-pat-ops . pat-ops)))
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
                (ik-node-bs (cmplr-ast-node-builder))))
     (define new-info (combine-info info this-info))
     (cmplr header groups new-info))]
  #:methods gen:cmplr-node-body-builder
  [(define build-node-body-stx ast-node-body-builder)])

(struct cmplr-ast-node-builder []
  #:methods gen:cmplr-node-builder
  [(define (build-node-stx canb node node-spec-state)
     (printf "cmplr-ast-node-builder: ~a\n" node)
     (match-define (cmplr:node pat-stx dirs body-stx) node)
     (match-define (cmplr:spec-state:node cspec gspec nspec) node-spec-state)
     (match-define (cmplr (cmplr:header id args (cmplr:header:type cfrom cto)) groups info) cspec)
     (define-values (new-dirs new-body-stx) (update-stx-with-dirs dirs body-stx))
     (cmplr:node pat-stx new-dirs new-body-stx))])
