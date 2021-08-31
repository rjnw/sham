#lang racket

(require syntax/parse
         racket/syntax
         racket/generic
         (for-template racket))

(require "pat.rkt"
         "ooo.rkt"
         "kw-info.rkt"
         "utils.rkt"
         "generics.rkt"
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

(struct cmplr:pat:compiled-var pat:var [ast-type body-var]
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax cv)
     (match-define (cmplr:pat:compiled-var var ast-type body-var) cv)
     var)])

(struct cmplr:pat:ast-node pat:app []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax pan)
     (match-define (cmplr:pat:ast-node op rands) pan)
     #`(#,(get-fid (ast:basic-id op)) #,@(to-syntax rands)))])


(struct cmplr:pat:ooo pat:ooo []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax poo)
     (match-define (cmplr:pat:ooo p k) poo)
     #`(#,(to-syntax p) #,(oook-syntax k)))])

(struct cmplr:pat:seq pat:seq []
  #:methods gen:stx-construct
  [(define/generic to-syntax ->syntax)
   (define (->syntax pse)
     (match-define (cmplr:pat:seq ps) pse)
     #`(#,@(map to-syntax ps)))])

(define (compile-ast-type var ast-type cstate)
  (match-define (cmplr:state:node cspec gspec nspec args) cstate)
  (match-define (cmplr header groups info) cspec)
  (define extra-cargs (map cdr args))
  (define (perform-compile cmplr-group depth)
    (match-define (cmplr:group id gtype nodes ginfo) cmplr-group)
    (match depth
      [0 #`(#,id #,var #,@extra-cargs)]
      [`((,mn . ,mx) . 0)
       #`(map (curryr #,id #,@extra-cargs) #,var)]
      [else (error 'sham/sam/TODO "compiler deeper ~a ~a" var depth)]))
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
     (perform-compile ast-group depth)]))

(struct auto-compile-pattern [stxid]
  #:methods gen:cmplr-bind-operator
  [(define (bind-operator-identifier acp) (auto-compile-pattern-stxid acp))
   (define (bind-operator-parse-syntax acp body ast-type)
     ;; (printf "parse-pattern-syntax: ~a ~a\n" ast-parse body)
     ;; TODO maybe use pattern and path instead of ast-type for generalizing values
     (match body
       [(list var) (cmplr:pat:compiled-var (generate-temporary var) ast-type var)]
       [else (error 'sham/sam "error in auto compile pattern, can only have one identifier: ~a" body)]))
   (define (bind-operator-gen-syntax acp pat state)
     (match-define (cmplr:pat:compiled-var match-stx ast-type bound-var) pat)
     (values #`(define #,bound-var #,(compile-ast-type match-stx ast-type state))
             state))])

(struct basic-node-builder []
  #:methods gen:cmplr-node-builder
  [(define (build-cmplr-node bnb stxc cspec gspec nspec)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cid cargs ctyp) header)
     (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
     (match-define (cmplr:type gfrom gto) gtyp)
     (match-define (cmplr:node bind bodys) nspec)

     (define from-ast-spec (info-1value 'from-ast-spec info))
     (define bind-operators (info-value 'bind-operators info '()))
     (define body-operators (info-value 'body-operators info '()))

     (define from-group-ast-spec
       (cond
         [(find-group/node-spec gfrom from-ast-spec) => cdr]
         [else (error 'sham/sam "couldn't locate ast group for compiling: ~a" gfrom)]))

     (define (find-operator stxid)
       (find-first
        bind-operators
        (λ (bo)
          (define op-id (bind-operator-identifier bo))
          (free-identifier=? op-id stxid))))
     (define (find-body-operator stxid)
       (find-first
        body-operators
        (λ (bo)
          (define op-id (body-operator-identifier bo))
          (free-identifier=? op-id stxid))))

     ;; (stx pat -> (values (listof vars) stx)) (listof stx) -> (values (listof vars) (listof stx))
     ;; special fold over two lists with 2 values returning
     (define (fold-seq f lst)
       (for/fold ([vars '()]
                  [stx '()])
                 ([sub lst])
         (define-values (sub-vars sub-stx) (f sub))
         (values (append sub-vars vars)
                 (if sub-stx (cons sub-stx stx) stx))))

     ;; (struct pat-stk [bvars pat] #:prefab)
     (define ((cmplr-pattern stx ast-type) op)
       (let ([op-pat (bind-operator-parse-syntax op stx ast-type)])
         (if (pat:var? op-pat)
             (values (list (cons op-pat op)) op-pat)
             (values '() op-pat))))

     (define ((ast-pattern rst) ast-node-spec)
       (match-define (ast:node nid ninfo nargs npat) ast-node-spec)
       ;; (printf "ast-pattern:\n")
       ;; (pretty-print (pretty-node ast-node-spec))
       ;; (printf "~a\n\n" rst)

       (define (from-ast-type ast-type)
         (match ast-type
           [(ast:type:internal depth (cons grp subs))
            (define ast-group (find-group-spec grp from-ast-spec))
            (if ast-group
                (ast:type:internal depth (list ast-group from-ast-spec))
                (error 'sham/sam "couldn't find ast type: ~a" grp))]
           [(ast:type depth) ast-type]))
       (define-values (node-vars node-stx)
         (let rec ([parse (stx-path-for-pattern npat #`(#,@rst))])
           ;; (printf "rec: ~a\n" parse)
           ;; (stx-path-for-pattern npat #`(#,@rst))

           (match parse
             [`(seq ,subs ,(pat:seq (vector ps ...)))
              (fold-seq rec subs)]
             [`(var ,stx ,var-pat)
              (match-define (ast:pat:single id check) var-pat)
              (define node-arg (find-node-arg ast-node-spec id))
              (match-pattern stx (from-ast-type (ast:node:arg-type node-arg)))]
             [`(ooo ,subs ,pat)
              ;; (printf "ooo: ~a\n" subs)
              (fold-seq rec subs)]
             [`(___ ,s ,nx ,pat) (values '() s)]
             [`(dat #f ,_) (values '() #f)])))
       (values node-vars (cmplr:pat:ast-node ast-node-spec (flatten node-stx))))

     ;; -> (values bind-vars match-pat)
     (define (match-pattern stx maybe-ast-spec)
       ;; (printf "match-pattern: ~a\n" stx)
       (match (syntax-e stx)
         [(? symbol?) (values '() stx)]
         [(cons fst rst)
          (cond
            [(find-node-spec fst from-group-ast-spec from-ast-spec) => (ast-pattern rst)]
            [(find-operator fst) => (cmplr-pattern rst maybe-ast-spec)]
            [else
             ;; keep the pattern as is in fst but recur over rst
             (define-values (vars stxs) (fold-seq (curryr match-pattern maybe-ast-spec) rst))
             (values vars #`(#,fst #,@(map ->syntax stxs)))]
            ;; [else (error 'sham/sam "unknown sequence in bind pattern ~a ~a" fst rst)]
            )]))
     ;; (printf "\n\nparsing-node: ~a\n" bind)
     (define-values (bound-vars match-pat) (match-pattern bind from-group-ast-spec))
     (define pat-stx (->syntax match-pat))

     ;; (define bound-var-stx (map (λ (bv) (operator-body-syntax (cdr bv) (car bv) cspec gspec nspec)) bound-vars))
     ;; #`[#,(->syntax match-pat) #,@(->syntax (map ->syntax bound-vars)) #,@bodys]

     (define (bvars&body bvars raw-body-stxs)
       (define initial-cmplr-state (cmplr:state:node cspec gspec nspec (map (λ (a) (cons a a)) (map car cargs))))
       ;; f : val state -> (values result state)
       ;; fold over a list while keeping a state
       (define (fold/state f initial-state lst)
         (for/fold ([res '()]
                    [state initial-state])
                   ([val lst])
           (define-values (nval nstate) (f val state))
           (values (cons nval res) nstate)))

       (define-values (bvar-stxs bvar-state)
         (fold/state (λ (bv state) (bind-operator-gen-syntax (cdr bv) (car bv) state)) initial-cmplr-state bvars))

       (define (do-body body-stx body-state)
         (match (syntax-e body-stx)
           [(cons fst rst)
            (cond [(find-body-operator fst) => (λ (op) (body-operator-gen-syntax op rst do-body body-state))]
                  [else
                   (define-values (rst-stxs nstate) (fold/state do-body body-state rst))
                   (values (datum->syntax body-stx (cons fst rst-stxs))
                           nstate)])]
           [else (values body-stx body-state)]))

       (define-values (body-stxs body-state) (fold/state do-body bvar-state raw-body-stxs))
       (append bvar-stxs body-stxs))

     (define body-stx (->syntax (bvars&body bound-vars bodys)))
     #`[#,pat-stx #,@body-stx
        ;; #,@bound-var-stx #,@bodys
        ])])

(struct basic-group-builder []
  #:methods gen:cmplr-group-builder
  [(define (build-cmplr-group bgb stxc cspec gspec)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
     (match-define (cmplr:type gfrom gto) gtyp)
     #`(define (#,gid group-inp #,@(map car cmplr-args))
         (match group-inp
           #,@stxc)))])

(struct basic-top-builder []
  #:methods gen:cmplr-top-builder
  [(define (build-cmplr-top btb stxc cspec)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:type cfrom cto) cmplr-type)
     #`(define (#,cmplr-id cmplr-inp #,@(map list (map car cmplr-args) (map cdr cmplr-args)))
         #,@stxc
         (#,(cmplr:group-id (car groups)) cmplr-inp #,@(map car cmplr-args))))])

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
                                 (basic-node-builder)
                                 (basic-group-builder)
                                 (basic-top-builder))))
  (define (foldr-builders f base) (foldr f base builders))

  (define cmplr-spec (foldr-builders update-spec raw-cmplr-spec))

  (match-define (cmplr header groups info) cmplr-spec)
  (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
  (match-define (cmplr:type cfrom cto) cmplr-type)

  (define from-ast-spec (get-ast-spec cfrom))

  (define (do-group group-spec)
    (match-define (cmplr:group gid gtype gnodes ginfo) group-spec)
    (define (do-node node-spec)
      (foldr-builders (build-node cmplr-spec group-spec node-spec)
                      empty))
    (define node-stxs (map do-node gnodes))
    (foldr-builders (build-group cmplr-spec group-spec) node-stxs))

  (define group-stxs (map do-group groups))
  (define cmplr-stx (foldr-builders (build-top cmplr-spec) group-stxs))

  (printf "cmplr-stx: \n") (pretty-print (syntax->datum cmplr-stx))
  (values (list cmplr-stx) cmplr-spec))
