#lang racket

(require syntax/parse
         racket/syntax
         (for-template racket
                       syntax/parse))

(require "reqs.rkt"
         "utils.rkt"
         "pat.rkt"
         "pat-stx.rkt"
         "state.rkt"
         "basic.rkt"
         (for-template "runtime.rkt")
         "../../runtime/transform.rkt")

(provide (all-defined-out))

(define (internal-syntax-class stxid cspec)
  (define (is-group g)
    (match-define (cmplr:group id type nodes info) g)
    (and (equal? (->symbol stxid) (->symbol id))
         id))
  (locate-first (cmplr-groups cspec) is-group))

;; a normal syntax variable in pattern, corresponds to syntax-parse variables with : for specifying types
;;  binds the syntax with a generated id and creates a syntax variable directive as the value
;;  is bound in syntax space or with syntax attribute if local type
(struct stx-var-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op val state frec)
     (match-define (cmplr:state:node spec vdirs path) state)
     (match-define (cmplr:spec-state:node cspec gspec nspec) spec)
     (define (var-with-type var-id typ)
       (define gen-id (generate-temporary var-id))
       (define internal-class (internal-syntax-class typ cspec))
       (define var-type-stx
         (if internal-class
             #`(#,internal-class #,@(internal-class-args-stx (cmplr-args-id cspec)))
             typ))

       (define svar (cmplr:pat:stx:var gen-id var-id var-type-stx))
       (define sdir (cmplr:dir:stx:with (stx-cls-with-var var-id path)
                                        (stx-cls-attr-val gen-id (and internal-class result-attribute-stxid))))
       (values svar (cmplr:state:node spec (append vdirs (list sdir)) path)))

     ;; (printf "stx-var: ~a ~a\n" val state)
     (if (and (syntax? val) (identifier? val))
         (match (split-identifier val)
           [(list _ id typ) (var-with-type id typ)]
           [(list #f id) (values (cmplr:pat:stx:var id id #f) state)]
           [else (error 'sham/sam/TODO)])
         (values val state)))])

(struct stx-quote-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax sqo val state frec)
     ;; (printf "stx-quote: ~a ~a\n" val state)
     (define dat-val
       (syntax-parse val
         [((~datum quote) i:id) #`i]
         [else #f]))
     (if dat-val (values (cmplr:pat:stx:dat dat-val) state) (values val state)))])

(struct stx-~-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (define (special-op? stxe)
       (and (cons? stxe)
            (identifier? (car stxe))
            (regexp-match #rx"^~(.*)$" (symbol->string (syntax->datum (car stxe))))))
     (define (do-special-op stx)
       (match-define (cons fst rst) (syntax-e stx))
       (define-values (rst-stxs rst-state) (mapl/state frec state rst))
       (values (cmplr:pat:stx:op fst rst-stxs) rst-state))
     (if (and (syntax? stx)
              (not (identifier? stx))
              (special-op? (syntax-e stx)))
         (do-special-op stx)
         (values stx state)))])

(struct stx-id-pat-operator []
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (define (id-operator? stxe)
       (and (cons? stxe)
            (identifier? (car stxe))
            (regexp-match #rx"^id-.*$" (symbol->string (syntax->datum (car stxe))))
            ;; (member (syntax-e (car stxe)) '(id-def id-ref))
            ))
     (define (do-op)
       (match-define (cmplr:state:node spec vdirs path) state)
       (match-define (cmplr:spec-state:node cspec gspec nspec) spec)
       (syntax-parse stx
         [(id-op:id id-name:id ((~datum quote) id-kind))
          (define gen-id-name (generate-temporary #`id-name))
          (define id-dir (cmplr:dir:stx:with (stx-cls-with-var #`id-name path)
                                             (stx-cls-attr-val gen-id-name result-attribute-stxid)))
          (values (cmplr:pat:stx:op (format-id #`id-op "~~~a" #`id-op)
                                    (list gen-id-name #`'id-kind))
                  (cmplr:state:node spec (append vdirs (list id-dir)) path))]))
     (if (and (syntax? stx)
              (not (identifier? stx))
              (id-operator? (syntax-e stx)))
         (do-op)
         (values stx state)))])

(struct stx-ooo-seq-pat-operator [identifies? pat-type parser]
  #:methods gen:cmplr-operator
  [(define (operator-parse-syntax op stx state frec)
     (match-define (stx-ooo-seq-pat-operator identifies? pat-type parser) op)
     ;; (printf "stx-ooo:~a: ~a ~a\n" pat-type stx state)
     (define (do-list-stx pat-subs state)
       (match-define (cmplr:state:node (cmplr:spec-state:node cspec gspec nspec) bvids path) state)

       (define strict-parens? (info-value 'strict-parens (cmplr-info cspec)))
       (define paren-shape (syntax-property stx 'paren-shape))

       (define (frec-ooo v state)
         (match v
           [(? syntax?) (frec v state)]
           [(pat:ooo p k)
            (define new-state (add-ooo-stx-path state k))
            (define-values (pstx state^) (frec p new-state))
            (values (cmplr:pat:ooo pstx k)
                    ;; (list pstx #`(... ...))
                    (peel-ooo-stx-path state^))]
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
          (list (cmplr:dir:stx:kwrd #'#:unless
                                    (list #`(equal? (syntax-property #'#,stx-id 'paren-shape) #,paren-shape)))
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
  (basic-stx-rec pat-stx node-state (info-value ik-node-pat-ops info)))

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
(struct cmplr-val-src-stx-tgt []
  #:methods gen:cmplr-node-body-builder
  [(define (build-node-body-stx vssb body-stx state)
     (printf "val-src-stx-tgt: ~a ~a\n" body-stx state)
     (match-define (cmplr:state:node spec dirs path) state)
     (define bind-dirs (filter cmplr:dir:bind? dirs))
     (define other-dirs (filter-not cmplr:dir:bind? dirs))
     (values (combine-binds-with-syntax bind-dirs (stx:qs body-stx))
             (cmplr:state:node spec other-dirs path)))])

(struct cmplr-stx-type cmplr:type [maybe-of-ast]
  #:property prop:procedure (λ (_ of)
                              (match (syntax-e of)
                                [(list ss) (cmplr-stx-type (get-cmplr-type ss))]))
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cst curr-spec)
     (match-define (cmplr-stx-type maybe-of-ast) cst)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args (cmplr:header:type src tgt)) header)

     ;; (printf "cmplr-stx-type: src? ~a\n" (eqv? cst src))
     ;; (printf "\tinfo: ~a\n" info)
     (define actual-builder
       (if (eqv? cst src) (cmplr-stx-source maybe-of-ast) (cmplr-stx-target maybe-of-ast)))

     (define new-info
       (update-info ik-spec-bs (λ (bs) (append bs (list actual-builder))) info))

     ;; (printf "\tnew-info: ~a\n" new-info)
     (cmplr header groups new-info))])

(struct cmplr-stx-source [maybe-ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec css curr-spec)
     (match-define (cmplr-stx-source maybe-ast-spec) css)
     (match-define (cmplr header groups info) curr-spec)
     ;; (printf "cmplr-stx-source:\n")
     (define stx-ops
       (list
        (stx-seq-pat-operator)
        (stx-vec-pat-operator)
        (stx-quote-pat-operator)
        (stx-var-pat-operator)
        (stx-~-pat-operator)
        (stx-id-pat-operator)))

     (define this-info
       (kw-info (ik-node-pat-bs css)
                (ik-node-pat-ops . stx-ops)
                (ik-group-bs (cmplr-stx-class-group-builder))
                (ik-top-bs (cmplr-stx-class-top-builder))))
     (define new-info (combine-info info this-info))
     ;; (printf "new-info: \n") (pretty-info new-info)
     (cmplr header groups new-info))]
  #:methods gen:cmplr-node-pat-builder
  [(define build-node-pattern-stx syntax-node-pat-builder)])

(struct cmplr-stx-target [maybe-ast-spec]
  #:methods gen:cmplr-spec-updater
  [(define (update-cmplr-spec cst curr-spec)
     (match-define (cmplr-stx-target maybe-ast-spec) cst)
     (match-define (cmplr header groups info) curr-spec)
     (match-define (cmplr:header cmplr-id cmplr-args (cmplr:header:type src tgt)) header)
     (define (for-stx-src) curr-spec)
     (define (for-val-src)
       (define this-info
         (kw-info (ik-node-body-bs (cmplr-val-src-stx-tgt))))
       (cmplr header groups (combine-info info this-info)))
     (if (cmplr-stx-type? src) (for-stx-src) (for-val-src)))]
  #:methods gen:cmplr-node-body-builder
  [(define build-node-body-stx syntax-node-body-builder)])

(struct cmplr-stx-class-group-builder []
  #:methods gen:cmplr-group-builder
  [(define (build-group-stx scgb node-stxs cspec gspec)
     (match-define (cmplr:group gid gtype nodes info) gspec)
     (define node-parts (map (curry node-syntax-class-part cspec gspec) node-stxs))
     ;; TODO add cmplr-args
     (cmplr:stx:class gid node-parts (info-value 'splicing info)))])

(define (node-syntax-class-part cspec gspec nspec)
  (match-define (cmplr:node pat dirs body) nspec)
  (define stx-pat-dirs (filter cmplr:dir:stx? dirs))
  (define other-dirs (filter-not cmplr:dir:stx? dirs))
  (cmplr:stx:class:pat pat
                       (append stx-pat-dirs
                               (list
                                (cmplr:dir:stx:attr result-attribute-stxid
                                                    (combine-binds-with-let other-dirs body))))))

(struct cmplr-stx-class-top-builder []
  #:methods gen:cmplr-top-builder
  [(define (build-cmplr-top sctb stxc cspec)
     (match-define (cmplr-stx-class-top-builder) sctb)
     (match-define (cmplr header groups info) cspec)
     (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
     (match-define (cmplr:header:type cfrom cto) cmplr-type)
     (define cinp cmplr-input-stxid)
     (list
      #`(define (#,cmplr-id cmplr-inp #,@(map list (map car cmplr-args) (map cdr cmplr-args)))
          #,@(to-syntax stxc)
          (syntax-parse cmplr-inp
            [(~var #,cinp #,(cmplr:group-id (car groups)))
             (attribute #,(format-id cinp "~a.~a" cinp result-attribute-stxid))]))))])
