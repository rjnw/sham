#lang racket

(require syntax/parse
         racket/syntax
         racket/generic
         (for-template racket
                       racket/syntax
                       syntax/parse))

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
      (match-let* ([(cmplr:node pat body) node-spec]
                   [node-spec-state (cmplr:spec-state:node cmplr-spec group-spec node-spec)]
                   [initial-node-state (cmplr:state:node node-spec-state '())]
                   [(cons pat-stx pat-node-state) (foldr-builders build-node-pattern (cons pat initial-node-state))]
                   [(cons body-stx body-node-state) (foldr-builders build-node-body (cons body pat-node-state))]
                   [(cmplr:node:state _ vars&dirs) body-node-state]
                   [full-node-stx (append pat-stx vars&dirs body-stx)])
        (foldr-builders (curryr build-node node-spec-state) full-node-stx)))

    (foldr-builders (build-group cmplr-spec group-spec) (map do-node gnodes)))

  (define cmplr-stx (foldr-builders (build-top cmplr-spec) (map do-group groups)))

  (pretty-print-columns 160)
  (printf "cmplr-stx: \n") (pretty-print (syntax->datum cmplr-stx))

  (values cmplr-stx cmplr-spec))

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
