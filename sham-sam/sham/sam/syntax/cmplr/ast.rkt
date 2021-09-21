#lang racket

(require "reqs.rkt")

(define (ast-node-pat-builder pat-spec node-state)
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

(define (ast-node-body-builder body-spec node-state)

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

(struct cmplr-ast-type [of]
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
       (update-info 'builders (λ (bs) (append bs (list actual-builder))) info))
     (cmplr header groups new-info))])

(struct cmplr-ast-source cmplr-ast-type []
  #:methods gen:cmplr-node-pat-builder
  [(define build-node-pattern-stx ast-node-pat-builder)])
(struct cmplr-ast-target cmplr-ast-type []
  #:methods gen:cmplr:node-body-builder
  [(define build-node-pattern-stx ast-node-pat-builder)])
