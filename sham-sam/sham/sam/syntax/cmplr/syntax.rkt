#lang racket

(require syntax/parse
         racket/syntax)

(require "reqs.rkt"
         "utils.rkt"
         "pat.rkt"
         "pat-stx.rkt"
         "state.rkt"
         "basic.rkt")
(provide (all-defined-out))
;; a normal syntax variable in pattern, corresponds to syntax-parse variables with : for specifying types
;;  binds the syntax with a generated id and creates a syntax variable directive as the value
;;  is bound in syntax space or with syntax attribute if local type
(struct stx-var-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op val state frec)
     (match-define (cmplr:state:node spec vdirs path) state)
     (printf "stx-var: ~a ~a\n" val state)
     ;; (match-define (cmplr:spec-state:node cspec gspec nspec) spec)
     (if (and (syntax? val) (identifier? val))
         (match (split-identifier val)
           [(list _ id typ)
            (define gen-id (generate-temporary id))
            (define var-type (cmplr:stx:stype typ state))
            (define svar (cmplr:pat:stx:var gen-id id var-type))
            (define sdir (cmplr:dir:stx:var svar))
            (values svar (cmplr:state:node spec (cons sdir vdirs) path))]
           [(list #f id) (values (cmplr:pat:stx:var id id (cmplr:stx:stype #`unknown state)) state)]
           [else (error 'sham/sam/TODO)])
         (values val state))

     ;; (define (build-ws-def var val)
     ;;   (match-define (cmplr:pat:compiled-var var-stx from-stx ast-type) var)
     ;;   (define pat-stx
     ;;     (stx-cls-with-var var-stx
     ;;                       (match ast-type
     ;;                         [(ast:type:internal depth _) depth]
     ;;                         [else #f])))
     ;;   (cons pat-stx val))
     ;; (define (get-pat-args pat (depth #f))
     ;;   (match pat
     ;;     [(cmplr:pat:ast-node n args)
     ;;      (append-map get-pat-args args)]
     ;;     [(cmplr:pat:seq ps)
     ;;      (append-map get-pat-args ps)]
     ;;     [(cmplr:pat:var var gen)
     ;;      (list (stx:def (stx-cls-with-var var depth) gen))]
     ;;     [else (printf "build-normal-args: ~a\n" pat) '()]))
     ;; (define (swap-normal-var-stx pat)
     ;;   (match pat
     ;;     [(cmplr:pat:ast-node n args)
     ;;      (cmplr:pat:ast-node n (map swap-normal-var-stx args))]
     ;;     [(cmplr:pat:seq ps)
     ;;      (cmplr:pat:seq (map swap-normal-var-stx ps))]
     ;;     [(cmplr:pat:var var gen)
     ;;      (cmplr:pat:var gen var)]
     ;;     [else (printf "warn: swap-normal-var-stx: ~a\n" pat) pat]))
     ;; (match bodys
     ;;   [(list (stx:def vars vals) ... res)
     ;;    (cmplr:node:case
     ;;     (swap-normal-var-stx pat)
     ;;     (stx:local-def #`with-syntax
     ;;                    (append (get-pat-args pat)
     ;;                            (map build-ws-def vars vals))
     ;;                    (with-syntax ([res-stx res])
     ;;                      #'#`res-stx)))]
     ;;   [else (error 'sham/sam/cmplr "cannot match final node syntax ~a" bodys)])
     )])

(struct stx-quote-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax sqo val state frec)
     (printf "stx-quote: ~a ~a\n" val state)
     (define dat-val
       (syntax-parse val
         [((~datum quote) i:id) #`i]
         [else #f]))
     (if dat-val (values (cmplr:pat:stx:dat dat-val) state) (values val state)))])

(struct stx-~-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (printf "stx-special: ~a ~a\n" stx state)
     (define (fmap stx state) (error 'todo "do special for ooo"))
     (define-values (special-op rst)
       (match (syntax-e stx)
         [(cons (and fst (regexp #rx"^~(.*)$" (list _ actual))) rst) (values fst rst)]
         [else (values #f stx)]))
     (if special-op
         (let-values ([(arg-stxs args-state) (frec rst state)])
           (values (cmplr:pat:stx:op #`op (stx:forced-seq arg-stxs)) args-state))
         (values stx state)))])

(struct stx-ooo-seq-pat-operator [identifies? pat-type parser]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (stx-ooo-seq-pat-operator identifies? pat-type parser) op)
     (printf "stx-ooo:~a: ~a ~a\n" pat-type stx state)
     (define (do-list-stx pat-subs state)
       (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) bvids path) state)

       (define strict-parens? (info-value 'strict-parens (cmplr-info cspec)))
       (define paren-shape (syntax-property stx 'paren-shape))

       (define (frec-ooo v state)
         (match v
           [(? syntax?) (frec v state)]
           [(pat:ooo p k)
            ;; TODO update path in state
            (define new-state (add-ooo-stx-path state k))
            (define-values (pstx state^) (frec p state))
            (values (cmplr:pat:ooo pstx k) state^)]
           [else (error 'sham/sam/TODO)]))

       (define-values (pstxs pstate) (mapl/state frec-ooo state pat-subs))
       (define lst-pat (pat-type pstxs paren-shape))

       ;; TODO implement paren-shape pattern expanders
       (define stx-id (generate-temporary #'lst))
       (define and-type #`expr)
       (define stx-var (cmplr:pat:stx:var stx-id stx-id and-type))

       (define paren-fail
         (cmplr:pat:stx:op
          #`~fail
          (list (stx-cls-dir #'#:unless (list #`(equal? (syntax-property #'#,stx-id 'paren-shape) #,paren-shape)))
                #'#f)))
       (define and-pat (cmplr:pat:stx:op #`~and (list stx-var paren-fail lst-pat)))
       (values (if (and paren-shape strict-parens?) and-pat lst-pat) pstate))


     (if (identifies? stx state)
         (call-with-values (thunk (parser stx state)) do-list-stx)
         (values stx state)))])

(define (stx-seq-pat-operator)
  (define (lst-parser stx state)
    (values (syntax-parse stx [(s:maybe-ooo-pat ...) (attribute s.pat)]) state))
  (stx-ooo-seq-pat-operator (λ (stx state) (and (syntax? stx) (list? (syntax-e stx))))
                            cmplr:pat:stx:seq
                            lst-parser))
(define (stx-vec-pat-operator)
  (define (vec-parser stx state)
    (values (syntax-parse stx
              [#(s:maybe-ooo-pat ...) (attribute s.pat)])
            state))
  (stx-ooo-seq-pat-operator (λ (stx state) (and (syntax? stx) (vector? (syntax-e stx))))
                            cmplr:pat:stx:vec
                            vec-parser))

(define (syntax-node-pat-builder ctype pat-stx node-state)
  (match-define (cmplr-stx-source maybe-ast) ctype)
  (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) '() path) node-state)
  (match-define (cmplr (cmplr:header cid cargs (cmplr:header:type cfrom cto)) groups info) cspec)

  (printf "\nsyntax-node-pat-builder:\n")
  (pretty-print (syntax->datum pat-stx))

  (define-values (pat-res state-res) (basic-pat-rec pat-stx node-state (info-value ik-node-pat-ops info)))
  (printf "syntax-node-pat-builder:result= \n~a \n~a\n" pat-res state-res)
  (pretty-print (syntax->datum (->syntax pat-res)))
  (values pat-res state-res)

  ;; (define operators (info-value info node-pat-operators))

  ;; ;; returns value of ~op syntax operators ~seq, ~optional
  ;; (define (stx-parse-op s)
  ;;   (match (syntax->string s)
  ;;     [(regexp #rx"^~(.*)$" (list _ actual)) actual]
  ;;     [else #f]))
  ;; ;; -> (values stx (listof vars&dirs))
  ;; (define (parse-pattern stx depth)
  ;;   (define (do-list pats pat-type (force-paren #t))
  ;;     (define paren-shape (syntax-property stx 'paren-shape))
  ;;     (define-values (pstxs bvars) (fold-with-vars (curryr rec-pattern depth) (reverse pats)))
  ;;     (define lpat (pat-type pstxs paren-shape))
  ;;     (define stx-id (generate-temporary #'lst))
  ;;     (define and-type #`expr)
  ;;     (define stx-var (cmplr:pat:stx:var stx-id stx-id and-type))
  ;;     ;; TODO only do if string-paren in info
  ;;     (define paren-fail
  ;;       (cmplr:pat:stx:fail
  ;;        (stx-cls-dir #'#:unless (list #`(equal? (syntax-property #'#,stx-id 'paren-shape) #,paren-shape))) #'#f))
  ;;     (define and-pat (cmplr:pat:stx:and (list stx-var paren-fail lpat)))
  ;;     (values (if force-paren and-pat lpat) bvars))

  ;;   (syntax-parse stx
  ;;     [s:syn-pat (do-pattern (attribute s.pat) depth)]
  ;;     [d:dat-pat (do-pattern (attribute d.pat) depth)]
  ;;     [(sp:id p:maybe-ooo-pat ...)
  ;;      #:when (stx-parse-op #`sp)
  ;;      (do-list (attribute p.pat) (λ (ps shap) (cmplr:pat:stx:op #`sp ps)) #f)]
  ;;     [(p:maybe-ooo-pat ...) (do-list (attribute p.pat) cmplr:pat:stx:seq)]
  ;;     [#(p:maybe-ooo-pat ...) (do-list (attribute p.pat) cmplr:pat:stx:vec)]))

  ;; ;; -> (values stx (listof bound-vars))
  ;; (define (rec-pattern v depth)

  ;;   (match v
  ;;     [(? syntax?) (parse-pattern v depth)]
  ;;     [(pat:var stx)
  ;;      (match (split-identifier stx)
  ;;        [(list _ id typ)
  ;;         (define gen-id (generate-temporary id))
  ;;         (define var-type (cmplr:stx:stype typ depth (map car cargs)))
  ;;         (define svar (cmplr:pat:stx:var id gen-id var-type))
  ;;         (values svar (list svar))]
  ;;        [else (match (syntax->string stx)
  ;;                [(regexp #rx"^~(.*)$" (list _ actual))
  ;;                 (values '() (cmplr:pat:stx stx))]
  ;;                [else (error 'sham/sam/cmplr "TODO stx:var ~a" stx)])])]
  ;;     [(pat:dat d) (values '() (cmplr:pat:stx:lit d))]
  ;;     [(pat:ooo p k)
  ;;      (define-values (pvars pstx) (rec-pattern p (cons k depth)))
  ;;      (values pvars (cmplr:pat:ooo pstx k))]
  ;;     [else (error 'sham/sam/cmplr "TODO")]))

  ;; (define-values (pat-stx bvars) (rec-pattern pat-spec #f))
  ;; (printf "vars&stx: ~a \n\t~a\n" bvars pat-stx)
  ;; (values pat-stx (cmplr:state:node cspec gspec nspec bvars))
  )

;; (define result-attr #`sres)

(define (syntax-node-body-builder ctype body-stx state)
  (error 'sham/sam/TODO)
  ;; (match-define (list dirs ... stx-result) body-spec)
  ;; (match-define (cmplr:state:node cspec gspec nspec bvars) node-state)
  ;; (match-define (cmplr header groups info) cspec)
  ;; (printf "syntax-node-body-builder\n")
  ;; (printf "bvars: ~a\n" bvars)
  ;; (define bvar-with-dirs
  ;;   (for/list ([bvar bvars])
  ;;     (match bvar
  ;;       [(cmplr:pat:stx:var id gen-id (cmplr:stx:stype typ-stx depth cargs))
  ;;        (define with-val
  ;;          (cond [(member (->symbol typ-stx) (map ->symbol (map cmplr:group-id groups)))
  ;;                 (stx-cls-attr-val gen-id result-attr)]
  ;;                [else (stx-cls-attr-val gen-id #f)]))
  ;;        (stx-cls-dir #`#:with (list (stx-cls-with-var id depth) with-val))]

  ;;       [else (error 'sham/sam/cmplr "unknown bound var for syntax class: ~a" bvar)])))
  ;; (define result-attr-dir (stx-cls-dir #`#:attr (list result-attr (stx:qs stx-result))))
  ;; `(,@bvar-with-dirs ,result-attr-dir)
  )

;; (struct syntax-class-group-builder [result-attr-stx]
;;   #:methods gen:cmplr-group-builder
;;   [(define (build-cmplr-group scgb stxc cspec gspec)
;;      (match-define (cmplr header groups info) cspec)
;;      (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
;;      (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
;;      (printf "syntax-group-builder: ~a\n" stxc)
;;      (printf "\t ginfo: ~a\n" ginfo)
;;      (define sc-args (map (compose stx-cls-arg car) cmplr-args))
;;      (define sc-bodys
;;        (for/list ([c stxc])
;;          (match-define (cmplr:node:case pat bodys) c)
;;          #`(pattern #,(->syntax pat) #,@(stx-seq bodys))))
;;      (define definer (if (info-value 'splicing ginfo) #`define-splicing-syntax-class #`define-syntax-class))
;;      (printf "done-with-syntax-group-builder\n")
;;      #`(#,definer (#,gid #,@sc-args) #,@sc-bodys))])

;; (struct syntax-class-top-builder [result-attr]
;;   #:methods gen:cmplr-top-builder
;;   [(define (build-cmplr-top sctb stxc cspec)
;;      (match-define (syntax-class-top-builder result-attr-stx) sctb)
;;      (match-define (cmplr header groups info) cspec)
;;      (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
;;      (match-define (cmplr:type cfrom cto) cmplr-type)
;;      (define cinp #'cinp)
;;      #`(define (#,cmplr-id cmplr-inp #,@(map list (map car cmplr-args) (map cdr cmplr-args)))
;;          #,@stxc
;;          (syntax-parse cmplr-inp
;;            [(~var #,cinp #,(cmplr:group-id (car groups)))
;;             (attribute #,(format-id cinp "~a.~a" cinp result-attr-stx))])))])

;; (define syntax-builders
;;   (list (basic-node-builder syntax-node-pat-builder syntax-node-body-builder)
;;         (syntax-class-group-builder result-attr)
;;         (syntax-class-top-builder result-attr)))

(struct cmplr-stx-type cmplr:type [maybe-of-ast]
  #:property prop:procedure (λ (_ of)
                              (match (syntax-e of)
                                [(list ss) (cmplr-stx-type (get-cmplr-type ss))]))
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cst curr-spec)
     (match-define (cmplr-stx-type maybe-of-ast) cst)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args (cmplr:header:type src tgt)) header)

     (printf "cmplr-stx-type: src? ~a\n" (eqv? cst src))
     (printf "\tinfo: ~a\n" info)
     (define actual-builder
       (if (eqv? cst src) (cmplr-stx-source maybe-of-ast) (cmplr-stx-target maybe-of-ast)))


     (define new-info
       (update-info ik-spec-bs (λ (bs) (append bs (list actual-builder))) info)
       ;; (combine-info
       ;;  `(;; (operators . ,stx-ops)
       ;;    (builders . ,syntax-builders))
       ;;  (remove-info `(operators builders) info))
       )

     (printf "\tnew-info: ~a\n" new-info)
     ;; (define new-info (insert-info 'operators (stx-var-operator) info))
     (cmplr header groups new-info))])

(struct cmplr-stx-source [maybe-ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec css curr-spec)
     (match-define (cmplr-stx-source maybe-ast-spec) css)
     (match-define (cmplr header groups info) curr-spec)
     (printf "cmplr-stx-source:\n")
     (define stx-ops
       (list
        (stx-seq-pat-operator)
        (stx-vec-pat-operator)
        (stx-quote-pat-operator)
        (stx-var-pat-operator)
        (stx-~-pat-operator)))

     ;; (define with-ast-op
     ;;   (if maybe-ast-spec (cons (stx-ast-pat-operator maybe-ast-spec) stx-ops) stx-ops))
     (define new-info
       (update-info ik-node-pat-bs
                    (λ (bs) (append bs (list css)))
                    (update-info ik-node-pat-ops (λ (bs) (append bs stx-ops)) info)))
     (printf "\tnew-info:~a\n" new-info)
     (cmplr header groups new-info))]
  #:methods gen:cmplr-node-pat-builder
  [(define build-node-pattern-stx syntax-node-pat-builder)])

(struct cmplr-stx-target [maybe-ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cst curr-spec) curr-spec)]
  #:methods gen:cmplr-node-body-builder
  [(define build-node-body-stx syntax-node-body-builder)])
