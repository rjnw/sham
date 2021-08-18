#lang racket

(require syntax/parse)

(require "pat.rkt"
         "kw-info.rkt"
         "utils.rkt"
         (submod "spec.rkt" ast)
         (submod "spec.rkt" compiler)
         (submod "class.rkt" compiler)
         (submod "class.rkt" pat)
         (submod "generics.rkt" compiler))

(provide build-compiler-syntax)

(define (get-ast-spec syn)
  (define-values (spec-value ff) (syntax-local-value/immediate syn))
  (unless (ast? spec-value) (error 'sham:sam "unknown ast specification ~a:~a" syn spec-value))
  spec-value)

(struct basic-node-builder []
  #:methods gen:cmplr-node-builder
  [(define (build-cmplr-node bnb stxc spec gspec nspec)
     (match-define (cmplr header groups info) spec)
     (define from-ast-spec (info-1value 'from-ast-spec info))
     (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
     (match-define (cmplr:node bind bodys) nspec)
     (define (parse-bind bind-stx)
       (syntax-parse #`(#,bind-stx)
         [(b:any-pat) (attribute b.pat)]))
     (define (zf val pat path)
       (match pat
         [(pat:var s) s]
         [(pat:dat d) d]
         [(pat:seq ps)
          (match ps
            [(vector fst rst ...)
             (cond [(find-node fst from-ast-spec) => do-ast-node]
                   [(find-operator fst) => do-operator]
                   [else TODO])])]
         [(pat:alt ps) ps]
         [(pat:ooo p k) p]
         [(pat:app o r) r])
       (printf "zf: ~a ~a ~a\n" val pat path))
     (pat-zipper zf '_ (parse-bind bind))

     ;; (define-values (match-pattern bound-vars) (do-bind bind))
     ;; (define match-body (do-body bodys bound-vars))
     ;; #`[#,match-pattern #,match-body]
     stxc)])

(struct basic-group-builder []
  #:methods gen:cmplr-group-builder
  [(define (build-cmplr-group bgb stxc spec gspec)
     stxc)])

(struct basic-top-builder []
  #:methods gen:cmplr-top-builder
  [(define (build-cmplr-top btb stxc spec)
     stxc)])

(struct ast-spec-info []
  #:methods gen:cmplr-spec-builder
  [(define (update-cmplr-spec asi curr-spec)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:type cfrom cto) cmplr-type)
     (define from-ast-spec (get-ast-spec cfrom))
     (define from-ast-spec (get-ast-spec cto))

     (cmplr header groups (add-info 'from-ast-spec from-ast-spec info)))])

(define (build-compiler-syntax raw-cmplr-spec)

  ;TODO get from info + defaults
  (define builders (append (cmplr-info raw-cmplr-spec)
                           (list (add-ast-spec-info)
                                 (basic-node-builder))))
  (define (foldr-builders f base) (foldr f base builders))

  (define cmplr-spec (foldr-builders update-spec raw-cmplr-spec))

  (match-define (cmplr header groups info) cmplr-spec)
  (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
  (match-define (cmplr:type cfrom cto) cmplr-type)

  (define from-ast-spec (get-ast-spec cfrom))

  (define (do-group group-spec)
    (match-define (cmplr:group gid gtype gnodes ginfo) group-spec)
    (printf "do-group: ~a\n" group-spec)
    (define (do-node node-spec)
      (foldr-builders (build-node cmplr-spec group-spec node-spec)
                      empty))
    (define node-stxs (append-map do-node gnodes))
    (foldr-builders (build-group cmplr-spec group-spec)
                    node-stxs))

  (define group-stxs (map do-group groups))
  (define cmplr-stx (foldr-builders (build-top cmplr-spec)
                                    group-stxs))

  ;; (define (do-group group-spec)
  ;;   (match-define (cmplr:group name type nodes info) group-spec)

  ;;   (define (do-node node-spec)
  ;;     (match-define (cmplr:node binding-pattern bodys) node-spec)
  ;;     (define (is-legal-op op) void)
  ;;     (define ((apply-pattern op) rands) void)
  ;;     (define (parse-node-expr e p)
  ;;       (let rec ([stx e]
  ;;                 [path p])
  ;;         (match (parse-for-ooo stx)
  ;;           [(cons op rands)
  ;;            (cond [(is-legal-op op) => (apply-pattern rands)]
  ;;                  [else (cmplr:pat:app op (map (λ (r) (rec r `(app ,op ,rands ,path))) rands))])]
  ;;           [(pat:ooo ns cnt) (cmplr:pat:ooo (rec ns `(ooo ,ns ,cnt ,path)) cnt)]
  ;;           [e e])))
  ;;     (define (parse-node-bodys bs binding-path)
  ;;       (for/fold ([out '()])
  ;;                 ([be bs] [i (length bs)])
  ;;         (parse-node-expr be `(body ,i ,be ,out ,binding-path))))

  ;;     (define node-bindings (parse-node-expr binding-pattern))
  ;;     (define node-bodys (parse-node-bodys bodys `(binding ,node-bindings)))
  ;;     (define parsed-node-spec (cmplr:node node-binding node-bodys))

  ;;     (foldr-builders (build-node cmplr-spec group-spec parsed-node-spec) empty))

  ;;   (define node-defs (map do-node nodes))
  ;;   (define group-def (foldr-builders (build-group cmplr-spec group-spec) empty))

  ;;   (append node-defs group-def))

  ;; (define body-def (foldr-builders (build-body cmplr-spec) empty))
  ;; (define cmplr-stx (map ->syntax (append (append-map do-group groups) (list body-def))))
  (values cmplr-stx cmplr-spec)

  )
