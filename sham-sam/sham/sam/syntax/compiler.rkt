#lang racket

(require syntax/parse
         racket/syntax)

(require "pat.rkt"
         "kw-info.rkt"
         "utils.rkt"
         (submod "spec.rkt" ast)
         (submod "spec.rkt" compiler)
         (submod "class.rkt" compiler)
         (submod "class.rkt" pat)
         (submod "generics.rkt" compiler)

         (prefix-in rt: (for-template "../runtime/compiler.rkt")))

(provide build-compiler-syntax)

(define (get-ast-spec syn)
  (define-values (spec-value ff) (syntax-local-value/immediate syn))
  (unless (ast? spec-value) (error 'sham:sam "unknown ast specification ~a:~a" syn spec-value))
  spec-value)

(struct cmplr:pat:compiled-var pat:var [ast-type body-var])
(struct cmplr:pat:ast-node pat:app [])

(struct auto-compile-pattern [stxid]
  #:methods gen:cmplr-bind-operator
  [(define (operator-identifier acp) (auto-compile-pattern-stxid acp))
   (define (operator-parse-syntax acp body ast-type)
     ;; (printf "parse-pattern-syntax: ~a ~a\n" ast-parse body)
     (match body
       [(list var) (cmplr:pat:compiled-var (generate-temporary var) ast-type var)]
       [else (error 'sham/sam "error in auto compile pattern, can only have one identifier: ~a" body)]))])

(struct basic-node-builder []
  #:methods gen:cmplr-node-builder
  [(define (build-cmplr-node bnb stxc spec gspec nspec)
     (match-define (cmplr header groups info) spec)
     (define from-ast-spec (info-1value 'from-ast-spec info))
     (define bind-operators (info-value 'bind-operators info '()))

     (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
     (match-define (cmplr:type gfrom gto) gtyp)
     (define from-group-ast-spec
       (cond
         [(find-group/node-spec gfrom from-ast-spec) => cdr]
         [else (error 'sham/sam "couldn't locate ast group for compiling: ~a" gfrom)]))
     (match-define (cmplr:node bind bodys) nspec)
     (define (parse-bind bind-stx)
       (if (pat? bind-stx)
           bind-stx
           (syntax-parse #`(#,bind-stx)
             [(b:any-pat) (attribute b.pat)])))
     (define (find-operator stxid)
       (find-first
        bind-operators
        (λ (bo)
          (define op-id (operator-identifier bo))
          (free-identifier=? op-id stxid))))

     ;; (struct pat-stk [bvars pat] #:prefab)
     (define ((cmplr-pattern stx ast-type) op)
       (printf "cmplr-pattern: ~a ~a ~a\n" stx ast-type op)
       (let ([op-pat (operator-parse-syntax op stx ast-type)])
         (if (pat:var? op-pat)
             (values (list (cons op-pat op)) op-pat)
             (values '() op-pat))))

     (define ((ast-pattern rst) ast-node-spec)
       (match-define (ast:node nid ninfo nargs npat) ast-node-spec)
       (define-values (node-vars node-stx)
         (let rec ([ast-parse (stx-path-for-pattern npat #`(#,@rst))])
           (printf "ast-pattern/rec: ~a\n" ast-parse)
           (match ast-parse
             [`(seq (,subs ...) ,seq-pat)
              (for/fold ([vars '()]
                         [stx '()])
                        ([sub subs])
                (define-values (sub-vars sub-stx) (rec sub))
                (values (append sub-vars vars)
                        (if sub-stx (cons sub-stx stx) stx)))]
             [`(var ,stx ,var-pat)
              (match-define (ast:pat:single id check) var-pat)
              (define node-arg (find-node-arg ast-node-spec id))
              (printf "ast-pattern/node-arg: ~a\n" node-arg)
              (gen-pattern stx (ast:node:arg-type node-arg))]
             [`(dat #f ,_) (values '() #f)])))
       (values node-vars (cmplr:pat:ast-node ast-node-spec node-stx)))

     ;; -> (values bind-vars match-pat)
     (define (gen-pattern stx maybe-ast-spec)
       (printf "do-pattern: ~a ~a\n" stx maybe-ast-spec)
       (match (parse-bind stx)
         [(pat:var s) (values '() s)]
         [(pat:dat d) (values '() d)]
         [(pat:seq ps)
          (match ps
            [(vector fst rst ...)
             (cond [(find-node-spec fst from-group-ast-spec from-ast-spec) => (ast-pattern rst)]
                   [(find-operator fst) => (cmplr-pattern rst maybe-ast-spec)]
                   [else (error 'sham/sam "unknown sequence in bind pattern ~a ~a" fst rst)])])]
         [(pat:alt ps) (error 'sham/sam/TODO "pat:alt")]
         [(pat:ooo p k) (error 'sham/sam/TODO "pat:ooo")]
         [(pat:app o r) (error 'sham/sam/TODO "pat:app")]))
     (define-values (bound-vars match-pat) (gen-pattern bind from-group-ast-spec))
     (printf "bound-vars: ~a, match-pat: ~a\n" bound-vars match-pat)
     ;; #`[#,(->syntax match-pat) #,@(->syntax (map ->syntax bound-vars)) #,@bodys]
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
     ;; (define to-ast-spec (get-ast-spec cto))

     (cmplr header groups
            (add-info 'bind-operators (auto-compile-pattern #`rt:^)
                      (add-info 'from-ast-spec from-ast-spec
                                info))))])

(define (build-compiler-syntax raw-cmplr-spec)

  ;TODO get from info + defaults
  (define builders (append (cmplr-info raw-cmplr-spec)
                           (list (ast-spec-info)
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
