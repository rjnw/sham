#lang racket

(require "reqs.rkt"
         "utils.rkt"
         "state.rkt")

(provide (all-defined-out))
(struct ast-pat-node-operator [ast-spec]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (ast-pat-node-operator ast-spec) op)
     (define group-ast (get-current-ast-group state))
     (define (is-ast-node? stxid)
       ;; check if it is a fully formatted node identifier, or a short form from the current group
       #f)
     (define ast-type
       (and (syntax? stx)
            (match (syntax-e stx)
              [(cons fst rst) (is-ast-node? fst)]
              [id (is-ast-node? id)])))
     (if ast-type
         (error 'TODO)
         (values stx state)))])

(define (compile-ast-type var ast-type cstate)
  (printf "compile-ast-type: ~a ~a\n" var ast-type)
  (error 'TODO)
  ;; (match-define (cmplr:state:node cspec gspec nspec bvars) cstate)
  ;; (match-define (cmplr header groups info) cspec)
  ;; (match-define (cmplr:header cid cargs ctyp) header)
  ;; (define extra-cargs (map cdr cargs))
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
     (printf "ast-pat-compile-var-operator: ~a ~a\n" stx state)
     ;; TODO maybe use pattern and path instead of ast-type for generalizing values
     ;; (match body
     ;;   [(list var) (cmplr:pat:compiled-var (format-id var "v-~a" var) var ast-type)]
     ;;   [else (error 'sham/sam "error in auto compile pattern, can only have one identifier: ~a" body)])
     )
   ;; (define (bind-operator-gen-syntax acp pat state)
   ;;   (match-define (cmplr:pat:compiled-var match-stx bound-var ast-type) pat)
   ;;   (values (stx:def (cmplr:pat:compiled-var bound-var match-stx ast-type)
   ;;                    (compile-ast-type match-stx ast-type state)) state))
   ])

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

(define (ast-node-pat-builder ctype pat-stx state)

  (match-define (cmplr-ast-source ast-spec) ctype)
  (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) '() path) state)
  (match-define (cmplr (cmplr:header cid cargs (cons cfrom cto)) groups info) cspec)

  (println "\n\nast-node-pat-builder:")
  (pretty-print (syntax->datum pat-stx))
  (values pat-stx state)

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

(define (ast-node-body-builder ctype body-stx state)
  (values body-stx state)
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

(struct cmplr-ast-type cmplr:type [of]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cat curr-spec)
     (match-define (cmplr-ast-type of-ast) cat)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args (cmplr:header:type src tgt)) header)
     (printf "cmplr-ast-type: src? ~a\n" (eqv? cat src))
     (printf "\tinfo: ~a\n" info)

     (define actual-builder
       (if (eqv? cat src)
           (cmplr-ast-source of-ast)
           (cmplr-ast-target of-ast)))
     (define new-info
       (update-info ik-spec-bs (λ (bs) (append bs (list actual-builder))) info))
     (printf "\tnew-info: ~a\n" new-info)
     (cmplr header groups new-info))])

(struct cmplr-ast-source [ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cas curr-spec)
     (match-define (cmplr header groups info) curr-spec)
     (printf "cmplr-ast-source:\n")
     (define pat-ops (list))
     (define new-info (update-info ik-node-pat-ops (λ (bs) (append bs pat-ops)) info))
     (cmplr header groups new-info))]
  #:methods gen:cmplr-node-pat-builder
  [(define build-node-pattern-stx ast-node-pat-builder)])

(struct cmplr-ast-target [ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cat curr-spec)
     (printf "cmplr-ast-target:\n")
     curr-spec)]
  #:methods gen:cmplr-node-body-builder
  [(define build-node-pattern-stx ast-node-pat-builder)])
