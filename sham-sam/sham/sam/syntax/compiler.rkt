#lang racket

(require syntax/parse
         racket/syntax
         racket/generic
         (for-template racket
                       racket/syntax))

(require
 "pat.rkt"
 "ooo.rkt"
 "kw-info.rkt"
 "utils.rkt"
 "generics.rkt"
 "stx-constructs.rkt"
 "spec.rkt"
 (submod "spec.rkt" ast)
 (submod "spec.rkt" compiler)
 (submod "class.rkt" compiler)
 (submod "class.rkt" pat)
 (submod "generics.rkt" compiler)

 (prefix-in rt: (for-template "../runtime/compiler.rkt")))

(provide build-compiler-syntax
         rkt-syntax-cmplr)

(define (get-cmplr-type stx)
  (syntax-parse stx
    [name:id (lookup-spec stx)]
    [(op:id . args)
     ((lookup-spec #`op) (attribute args))]))

;; used for normal variables in pattern
;;   for basic builders this does nothing but keep generate-temporary id along
;;   for syntax builder we swap the syntax in pattern and bind the original in with-syntax
(struct cmplr:pat:var pat:var [gen-var]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax pv)
     (match-define (cmplr:pat:var var-stx gen-stx) pv)
     (to-syntax var-stx))])

(struct cmplr:pat:compiled-var cmplr:pat:var [type]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax cv)
     (match-define (cmplr:pat:compiled-var var gen-var ast-type) cv)
     (to-syntax var))])

(struct cmplr:pat:ast-node pat:app []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax pan)
     (match-define (cmplr:pat:ast-node op rands) pan)
     ;; (printf "ast-node: ~a\n" rands)
     (define rands-stx
       (match rands
         [(list (cmplr:pat:seq ps))
          (to-syntax (flatten ps))]
         [else (to-syntax rands)]))
     #`(#,(get-fid (ast:basic-id op)) #,@rands-stx))])

(struct cmplr:pat:ooo pat:ooo []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax poo)
     (match-define (cmplr:pat:ooo p k) poo)
     (list (to-syntax p) (oook-syntax k)))])

(struct cmplr:node:case pat:app []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax cnc)
     (match-define (cmplr:node:case op rands) cnc)
     (stx-seq (to-syntax op) (to-syntax rands)))])

(struct cmplr:pat:seq pat:seq []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax pse)
     (match-define (cmplr:pat:seq ps) pse)
     #`(#,@(map to-syntax (flatten ps))))])

(define (compile-ast-type var ast-type cstate)
  ;; (printf "compile-ast-type: ~a ~a\n" var ast-type)
  (match-define (cmplr:state:node cspec gspec nspec bvars) cstate)
  (match-define (cmplr header groups info) cspec)
  (match-define (cmplr:header cid cargs ctyp) header)
  (define extra-cargs (map cdr cargs))
  (define (perform-compile cmplr-group depth)
    (match-define (cmplr:group id gtype nodes ginfo) cmplr-group)
    (let rec ([depth depth]
              [val var])
      (match depth
        [#f #`(#,id #,val #,@extra-cargs)]
        [`((,mn . ,mx) . ,rst)
         (define nval (generate-temporary var))
         #`(map (λ (#,nval) #,(rec rst nval)) #,val)
         ;; #`(map (curryr #,id #,@extra-cargs) #,val)
         ]
        [else (error 'sham/sam/TODO "compile deeper ~a ~a ~a" var depth id)])))
  (match ast-type
    [(ast:type:internal depth (list group top))
     (define group-fid (get-fid (ast:basic-id group)))
     (define group-oid (get-oid (ast:basic-id group)))
     (define (is-group? g)
       (match-define (cmplr:group id gtype nodes ginfo) g)
       (match-define (cmplr:type from to) gtype)
       (or (equal? (->symbol from) (->symbol group-fid))
           (equal? (->symbol from) (->symbol group-oid)) ))
     (define ast-group (find-first groups is-group?))
     (unless ast-group
       (error 'sham/sam/cmplr "no compiler group for ~a" group-fid))
     (perform-compile ast-group depth)]
    [(ast:type:identifier depth)
     #`(rt:identifier->syntax #,var #'#,var)]))

(struct auto-compile-pattern [stxid]
  #:methods gen:cmplr-bind-operator
  [(define (bind-operator-identifier acp) (auto-compile-pattern-stxid acp))
   (define (bind-operator-parse-syntax acp body ast-type)
     ;; (printf "parse-pattern-syntax: ~a ~a\n" ast-parse body)
     ;; TODO maybe use pattern and path instead of ast-type for generalizing values
     (match body
       [(list var) (cmplr:pat:compiled-var (format-id var "v-~a" var) var ast-type)]
       [else (error 'sham/sam "error in auto compile pattern, can only have one identifier: ~a" body)]))
   (define (bind-operator-gen-syntax acp pat state)
     (match-define (cmplr:pat:compiled-var match-stx bound-var ast-type) pat)
     (values (stx:def (cmplr:pat:compiled-var bound-var match-stx ast-type)
                      (compile-ast-type match-stx ast-type state)) state))])

(struct bind-var-pattern [stxid binder]
  #:methods gen:cmplr-body-operator
  [(define (body-operator-identifier bvp) (bind-var-pattern-stxid bvp))
   (define (body-operator-gen-syntax bvp rst-stx frec cstate)
     (match-define (bind-var-pattern id binder) bvp)
     (match-define (cmplr:state:node cspec gspec nspec bvars) cstate)
     (match-define (cmplr header groups info) cspec)
     (syntax-parse rst-stx
       [([vars vals] ... bodys ...)
        (let*-values ([(vars-stx vars-state) (mapr/state frec cstate (attribute vars))]
                      [(vals-stx vals-state) (mapr/state frec vars-state (attribute vals))]
                      [(bodys-stx bodys-state) (mapr/state frec vals-state (attribute bodys))])
          (values (binder (map stx:def vars-stx vals-stx) bodys-stx) bodys-state))]))])

;; builds a cmplr:node:case stx object by using pat-builder and body-builder values
;;  also runs the genral node-syntax-operator at the end on the built case with cmplr:node:state
;;  algorithm keeps a cmplr:node:state around which gets updated by both of these functions
;; pat-builder: pat-spec cmplr-node-state -> pat-stx cmplr-node-state
;; body-builder: body-spec cmplr-node-state -> body-stx
(struct basic-node-builder [pat-builder body-builder]
  #:methods gen:cmplr-node-builder
  [(define (build-cmplr-node bnb stxc cspec gspec nspec)
     (match-define (basic-node-builder patb bodyb) bnb)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cid cargs ctyp) header)
     (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
     (match-define (cmplr:type gfrom gto) gtyp)
     (match-define (cmplr:node pats bodys) nspec)
     (define initial-state (cmplr:state:node cspec gspec nspec '()))
     (define-values (pat-stx pat-state) (patb pats initial-state))
     (define body-stx (bodyb bodys pat-state))

     (define all-operators (flatten (info-values 'operators info)))
     (foldr cmplr-operator-node-syntax
            (cmplr:node:case pat-stx body-stx)
            (filter cmplr-node-operator? all-operators)))])

(struct basic-group-builder [def-builder body-builder]
  #:methods gen:cmplr-group-builder
  [(define (build-cmplr-group bgb stxc cspec gspec)
     (match-define (basic-group-builder def-builder body-builder) bgb)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
     (define group-input #`group-input)
     (def-builder gid (cons group-input (map car cmplr-args)) (body-builder group-input stxc))
     ;; #`(define (#,gid #,group-input #,@(map car cmplr-args))
     ;;     #,(body-builder group-input stxc)
     ;;     ;; (#,match-stx group-inp #,@stxc)
     ;;     )
     )])

(struct basic-top-builder []
  #:methods gen:cmplr-top-builder
  [(define (build-cmplr-top btb stxc cspec)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:type cfrom cto) cmplr-type)
     #`(define (#,cmplr-id cmplr-inp #,@(map list (map car cmplr-args) (map cdr cmplr-args)))
         #,@stxc
         (#,(cmplr:group-id (car groups)) cmplr-inp #,@(map car cmplr-args))))])

(define (build-compiler-syntax raw-cmplr-spec)
  (define cmplr-spec (initial-spec-update raw-cmplr-spec))

  (match-define (cmplr header groups info) cmplr-spec)
  (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
  (match-define (cmplr:type cfrom cto) cmplr-type)

  ;; (printf "cmplr-info: ~a\n" info)
  (define builders (flatten (info-values 'builders info)))
  (define (foldr-builders f base) (foldr f base builders))

  (define (do-group group-spec)
    (match-define (cmplr:group gid gtype gnodes ginfo) group-spec)
    (define (do-node node-spec)
      (foldr-builders (build-node cmplr-spec group-spec node-spec) node-spec))
    (define node-stxs (map do-node gnodes))
    (foldr-builders (build-group cmplr-spec group-spec) node-stxs))

  (define group-stxs (map do-group groups))
  (define cmplr-stx (foldr-builders (build-top cmplr-spec) group-stxs))

  (printf "cmplr-stx: \n") (pretty-print (syntax->datum cmplr-stx))
  (values (list cmplr-stx) cmplr-spec))

(define (cmplr-spec-add-extra-info curr-spec extra-info)
  (match-define (cmplr header groups info) curr-spec)
  (cmplr header groups (append extra-info info)))

(struct extra-info-spec-builder [info]
  #:methods gen:cmplr-spec-builder
  [(define (update-cmplr-spec eisb curr-spec)
     (match-define (extra-info-spec-builder extra-info) eisb)
     (cmplr-spec-add-extra-info curr-spec extra-info))])

(struct add-type-specs []
  #:methods gen:cmplr-spec-builder
  [(define (update-cmplr-spec ats cspec)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:type cfrom cto) cmplr-type)
     (define from-type (get-cmplr-type cfrom))
     (define to-type (get-cmplr-type cto))
     (cmplr (cmplr:header cmplr-id cmplr-args (cmplr:type from-type to-type))
            groups
            (append `((from-spec . ,from-type)
                      (to-spec . ,to-type))
                    info)))])

(define ((find-operator operators get-identifier) stxid)
  (define (is-op? bo) (free-identifier=? (get-identifier bo) stxid))
  (and (identifier? stxid) (find-first operators is-op?)))

;; (stx pat -> (values (listof vars) stx)) (listof stx) -> (values (listof vars) (listof stx))
;; special fold over two lists with 2 values returning
(define (fold-with-vars f lst)
  (for/fold ([vars '()] [stx '()]) ([sub lst])
    (define-values (sub-vars sub-stx) (f sub))
    (values (append sub-vars vars)
            (if sub-stx (cons sub-stx stx) stx))))

(define (basic-ast-node-pat-builder pat-spec node-state)
  (match-define (cmplr:state:node cspec gspec nspec bvars) node-state)
  (match-define (cmplr header groups info) cspec)
  (match-define (cmplr:header cid cargs ctyp) header)
  (match-define (cmplr:type from-spec to-spec) ctyp)
  (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
  (match-define (cmplr:type gfrom gto) gtyp)
  (match-define (cmplr:node pats bodys) nspec)

  (define all-operators (flatten (info-values 'operators info)))
  (define bind-operators (filter cmplr-bind-operator? all-operators))
  (define find-bind-operator (find-operator bind-operators bind-operator-identifier))

  (define ((cmplr-pattern stx ast-type) op)
    (let ([op-pat (bind-operator-parse-syntax op stx ast-type)])
      (if (pat:var? op-pat)
          (values (list (cons op-pat op)) op-pat)
          (values '() op-pat))))

  ;; -> (values (listof bound-vars) stx)
  (define ((ast-pattern rst) ast-node-spec)
    (match-define (ast:node nid ninfo nargs npat) ast-node-spec)
    ;; (printf "ast-pattern:\n")
    ;; (pretty-print (pretty-node ast-node-spec))
    ;; (printf "~a\n\n" rst)

    (define (from-ast-type ast-type)
      (match ast-type
        [(ast:type:internal depth (cons grp subs))
         (define ast-group (find-group-spec grp from-spec))
         (if ast-group
             (ast:type:internal depth (list ast-group from-spec))
             (error 'sham/sam "couldn't find ast type: ~a" grp))]
        [(ast:type depth) ast-type]))
    (define-values (node-vars node-stx)
      (let rec ([parse (stx-path-for-pattern npat #`(#,@rst))])
        ;; (printf "rec: ~a\n" parse)
        ;; (stx-path-for-pattern npat #`(#,@rst))

        (match parse
          [`(seq ,subs ,(pat:seq (vector ps ...)))
           (define-values (vars stxs) (fold-with-vars rec subs))
           (values vars (cmplr:pat:seq stxs))]
          [`(var ,stx ,var-pat)
           (match-define (ast:pat:single id check) var-pat)
           (define node-arg (find-node-arg ast-node-spec id))
           (do-pattern stx (from-ast-type (ast:node:arg-type node-arg)))]
          [`(ooo ,subs ,pat)
           ;; (printf "ooo: ~a\n" subs)
           (fold-with-vars rec subs)]
          [`(___ ,s ,nx ,pat) (values '() s)]
          [`(dat #f ,_) (values '() #f)])))
    (values node-vars (cmplr:pat:ast-node ast-node-spec (flatten node-stx))))

  ;; -> (values bind-vars match-pat)
  (define (do-pattern stx maybe-ast-spec)
    ;; (printf "bind-pattern: ~a\n" stx)
    (match (syntax-e stx)
      [(? symbol?) (cond
                     [(find-bind-operator #'#%var) => (cmplr-pattern stx maybe-ast-spec)]
                     [else (values '() (cmplr:pat:var stx (generate-temporary stx)))])]
      [(cons fst rst)
       (cond
         [(find-node-spec fst from-group-ast-spec from-spec) => (ast-pattern rst)]
         [(or (find-bind-operator fst) (find-bind-operator #'#%app)) => (cmplr-pattern stx maybe-ast-spec)]
         ;; [(find-bind-operator #'#%app) => (cmplr-pattern stx maybe-ast-spec)]
         [else
          ;; keep the pattern as is in fst but recur over rst
          (define-values (vars stxs) (fold-with-vars (curryr do-pattern maybe-ast-spec) rst))
          (values vars (stx:app fst stxs))]
         ;; [else (error 'sham/sam "unknown sequence in bind pattern ~a ~a" fst rst)]
         )]))

  (define from-group-ast-spec
    (cond
      [(find-group/node-spec gfrom from-spec) => cdr]
      [else (error 'sham/sam "couldn't locate ast group for compiling: ~a" gfrom)]))

  ;; (printf "\n\nparsing-node: ~a\n" bind)
  (define-values (pat-vars pat-stx) (do-pattern pat-spec from-group-ast-spec))
  (values pat-stx (cmplr:state:node cspec gspec nspec pat-vars)))

(define (basic-ast-node-body-builder body-spec node-state)

  (match-define (cmplr:state:node cspec gspec nspec bvars) node-state)
  (match-define (cmplr header groups info) cspec)
  (match-define (cmplr:header cid cargs ctyp) header)
  (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
  (match-define (cmplr:type from-spec to-spec) gtyp)
  (match-define (cmplr:node pats bodys) nspec)

  (define all-operators (flatten (info-values 'operators info)))
  (define body-operators (filter cmplr-body-operator? all-operators))
  (define find-body-operator (find-operator body-operators body-operator-identifier))

  (define (bvars&body bvars raw-body-stxs)
    (define-values (bvar-stxs bvar-state)
      (mapr/state (λ (bv state) (bind-operator-gen-syntax (cdr bv) (car bv) state)) node-state bvars))

    (define (do-body body-stx body-state)
      (match (syntax-e body-stx)
        [(cons fst rst)
         (cond [(or (find-body-operator fst) (find-body-operator #'#%app))
                => (λ (op) (body-operator-gen-syntax op body-stx do-body body-state))]
               [else
                (define-values (rst-stxs nstate) (mapr/state do-body body-state rst))
                (values (datum->syntax body-stx (cons fst rst-stxs))
                        nstate)])]
        [else (values body-stx body-state)]))

    (define-values (body-stxs body-state) (mapr/state do-body bvar-state raw-body-stxs))
    (append bvar-stxs body-stxs))
  (bvars&body bvars body-spec))

(define (basic-group-definer gid args body) (stx:def gid (stx:lam args body)))

(define basic-builders
  (list (basic-node-builder basic-ast-node-pat-builder basic-ast-node-body-builder)
        (basic-group-builder basic-group-definer stx:match)
        (basic-top-builder)))

(define default-info-spec-builder
  (extra-info-spec-builder `((operators ,(bind-var-pattern #`rt:with (λ (args) #`(let #,@args)))
                                        ,(auto-compile-pattern #`rt:^))
                             (builders . ,basic-builders))))
(define default-updaters (list default-info-spec-builder (add-type-specs)))

(define (initial-spec-update raw-cspec)
  (define (run-builders spec)
    (foldr update-spec spec (info-values 'builders (cmplr-info spec))))

  (let* ([def-cspec (foldr update-spec raw-cspec default-updaters)]
         [dinfo (cmplr-info def-cspec)]
         [from-spec (info-value 'from-spec dinfo)]
         [to-spec (info-value 'to-spec dinfo)]
         [spec-cspec (update-spec to-spec (update-spec from-spec def-cspec))]
         [builder-cspec (run-builders spec-cspec)])
    builder-cspec))

;; (struct syntax-node-builder []
;;   #:methods gen:cmplr-node-builder
;;   )
;; (struct syntax-group-builder []
;;   #:methods gen:cmplr-group-builder
;;   [(define (build-cmplr-group sgb stxc cspec gspec)
;;      )])

(struct stx-var-operator []
  #:methods gen:cmplr-node-operator
  [(define (cmplr-operator-node-syntax op stxs)
     (match-define (cmplr:node:case pat bodys) stxs)
     ;; (printf "stx-var-operator: ~a \n" stxs)
     (define (with-syntax-pat var depth)
       (match depth
         [#f (->syntax var)]
         [(cons (cons mn mx) rst)
          #`(#,(with-syntax-pat var rst) (... ...))]) )
     (define (build-ws-def var val)
       (match-define (cmplr:pat:compiled-var var-stx from-stx ast-type) var)
       (define pat-stx
         (with-syntax-pat var-stx
           (match ast-type
             [(ast:type:internal depth _) depth]
             [else #f])))
       (cons pat-stx val))
     (define (get-pat-args pat (depth #f))
       (match pat
         [(cmplr:pat:ast-node n args)
          (append-map get-pat-args args)]
         [(cmplr:pat:seq ps)
          (append-map get-pat-args ps)]
         [(cmplr:pat:var var gen)
          (list (stx:def (with-syntax-pat var depth)
                         gen))]
         [else (printf "build-normal-args: ~a\n" pat) '()]))
     (define (swap-normal-var-stx pat)
       (match pat
         [(cmplr:pat:ast-node n args)
          (cmplr:pat:ast-node n (map swap-normal-var-stx args))]
         [(cmplr:pat:seq ps)
          (cmplr:pat:seq (map swap-normal-var-stx ps))]
         [(cmplr:pat:var var gen)
          (cmplr:pat:var gen var)]
         [else (printf "warn: swap-normal-var-stx: ~a\n" pat) pat]))
     (match bodys
       [(list (stx:def vars vals) ... res)
        (cmplr:node:case
         (swap-normal-var-stx pat)
         (stx:local-def #`with-syntax
                        (append (get-pat-args pat)
                                (map build-ws-def vars vals))
                        (with-syntax ([res-stx res])
                          #'#`res-stx)))]
       [else (error 'sham/sam/cmplr "cannot match final node syntax ~a" bodys)]))])

(struct cmplr:pat:stx:var cmplr:pat:compiled-var []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax sv)
     (match-define (cmplr:pat:stx:var id gen-id type) sv)
     #`(~var #,(to-syntax gen-id) #,(to-syntax type)))])
(struct cmplr:pat:stx:lit pat:dat []
  #:methods gen:stx-construct
  [(define (->syntax sd)
     (match-define (cmplr:pat:stx:lit d) sd)
     #`(~literal #,d))])
(struct cmplr:pat:stx:seq cmplr:pat:seq [paren-shape]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define ->syntax to-syntax)])
(struct cmplr:pat:stx:vec cmplr:pat:stx:seq [])
(struct cmplr:stx:stype [tstx depth args])
(define (syntax-node-pat-builder pat-spec node-state)
  (printf "\n\nsyntax-node-pat-builder:\n")
  (pretty-print (syntax->datum pat-spec))

  (match-define (cmplr:state:node cspec gspec nspec '()) node-state)
  (match-define (cmplr header groups info) cspec)
  (match-define (cmplr:header cid cargs ctyp) header)

  ;; -> (values (listof bound-vars) stx)
  (define (do-pattern v depth)
    (define (parse-pattern stx)
      (syntax-parse stx
        [s:syn-pat (do-pattern (attribute s.pat) depth)]
        [d:dat-pat (do-pattern (attribute d.pat) depth)]
        [(p:maybe-ooo-pat ...)
         (define-values (bvars pstx) (fold-with-vars (curryr do-pattern depth) (reverse (attribute p.pat))))
         (printf "paren-shape: ~a\n" (syntax-property stx 'paren-shape))
         (values bvars (cmplr:pat:stx:seq pstx (syntax-property stx 'paren-shape)))]
        [#(p:maybe-ooo-pat ...)
         (define-values (bvars pstx) (fold-with-vars (curryr do-pattern depth) (attribute p.pat)))
         (printf "paren-shape: ~a\n" (syntax-property stx 'paren-shape))
         (values bvars (cmplr:pat:stx:vec pstx (syntax-property stx 'paren-shape)))]))
    (match v
      [(? syntax?) (parse-pattern v)]
      [(pat:var stx)
       (match (split-identifier stx)
         [(list _ id typ)
          (define gen-id (generate-temporary id))
          (define bvar (cmplr:stx:stype typ depth (map car cargs)))
          (define svar (cmplr:pat:stx:var id gen-id typ))
          (values (list svar) svar)]
         [else (values '() v)])]
      [(pat:dat d) (values '() (cmplr:pat:stx:lit d))]
      [(pat:ooo p k)
       (define-values (pvars pstx) (do-pattern p (cons k depth)))
       (values pvars (cmplr:pat:ooo pstx k))]
      [else (error 'sham/sam/cmplr "TODO")]))
  (define-values (bvars pat-stx) (do-pattern pat-spec 0))
  (printf "vars&stx: ~a \n\t~a\n" bvars pat-stx)
  (values pat-stx (cmplr:state:node cspec gspec nspec bvars)))

(define (syntax-node-body-builder body-spec node-state)
  body-spec)
(define (syntax-group-definer gid args body)
  (printf "syntax-group-definer: ~a\n" body)
  (error 'stop)
  body)
(define syntax-builders
  (list (basic-node-builder syntax-node-pat-builder syntax-node-body-builder)
        (basic-group-builder syntax-group-definer (λ (ginp stxc) stxc))
        (basic-top-builder)))

(struct rkt-syntax-cmplr spec [of]
  #:property prop:procedure (λ (_ of)
                              (match (syntax-e of)
                                [(list ss) (rkt-syntax-cmplr (get-cmplr-type ss))]))
  #:methods gen:cmplr-spec-builder
  [(define (update-cmplr-spec sct curr-spec)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:type cfrom cto) cmplr-type)
     (printf "rkst-syntax-cmplr-target&curr-info: ~a\n" info)
     (define stx-ops (list (auto-compile-pattern #`rt:^)))

     (define new-info
       (combine-info
        `((operators . ,stx-ops)
          (builders . ,syntax-builders))
        (remove-info `(operators builders) info)))
     ;; (define new-info (insert-info 'operators (stx-var-operator) info))
     (cmplr header groups new-info))])
