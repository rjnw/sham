#lang racket

(require syntax/parse
         racket/syntax
         racket/generic
         (for-template racket
                       racket/syntax
                       syntax/parse))

(require
 "kw-info.rkt"
 "utils.rkt"
 "generics.rkt"
 "stx.rkt"
 "spec.rkt"
 (submod "spec.rkt" ast)
 (submod "spec.rkt" compiler)
 (submod "class.rkt" compiler)
 (submod "class.rkt" pat)
 (submod "generics.rkt" compiler)

 "cmplr/utils.rkt"
 "cmplr/syntax.rkt"
 "cmplr/ast.rkt")

(provide build-transform-syntax
         (rename-out [cmplr-stx-type rkt-syntax-cmplr]))

(define (build-transform-syntax raw-cmplr-spec)
  (define cmplr-spec (initial-spec-update raw-cmplr-spec))

  (match-define (cmplr header groups info) cmplr-spec)
  (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
  (match-define (cmplr:header:type cfrom cto) cmplr-type)

  (define (builders typ) (flatten (info-values typ info)))
  (define (foldr-builders typ f base) (foldr f base (builders typ)))

  (define (do-group group-spec)
    (match-define (cmplr:group gid gtype gnodes ginfo) group-spec)
    (define (do-node node-spec)
      (match-define (cmplr:node pat dirs body) node-spec)
      (match-let*-values
          ([(node-spec-state) (cmplr:spec-state:node cmplr-spec group-spec node-spec)]
           [(initial-node-state) (cmplr:state:node node-spec-state dirs #f)]
           [(pat-stx pat-node-state)
            (foldr/state build-node-pattern-stx pat initial-node-state (builders ik-node-pat-bs))]
           [(body-stx body-node-state)
            (foldr/state build-node-body-stx body pat-node-state (builders ik-node-body-bs))]
           [((cmplr:state:node _ vars&dirs path)) body-node-state]
           [(full-node-stx) (cmplr:node pat-stx vars&dirs body-stx)])
        (foldr (curryr build-node-stx node-spec-state) full-node-stx (builders ik-node-bs))))

    (foldr-builders ik-group-bs (build-group cmplr-spec group-spec) (map do-node gnodes)))

  (define cmplr-stx (foldr-builders ik-top-bs (build-top cmplr-spec) (map do-group groups)))

  (pretty-print-columns 160) (printf "transform-stx: \n") (pretty-print (map syntax->datum cmplr-stx))

  (values cmplr-stx cmplr-spec))

(define (initial-spec-update raw-cspec)
  (define (run-builders spec)
    ;; (printf "updating-spec:\n\tspec-builders: ~a\n" (info-value ik-spec-bs (cmplr-info spec)))
    (foldr update-cmplr-spec spec (info-value ik-spec-bs (cmplr-info spec))))
  (define (build-cmplr-type t)
    (match t
      [(? cmplr:type?) t]
      [(? ast?) (cmplr-ast-type t)]
      [else (error 'sham/sam/transform "unknown compiler type: ~a" t)]))
  (match-let* ([(cmplr raw-header groups raw-info) raw-cspec]
               [(cmplr:header cid cargs (cmplr:header:type cfrom-stx cto-stx)) raw-header]
               [from-type (build-cmplr-type (get-cmplr-type cfrom-stx))]
               [to-type (build-cmplr-type (get-cmplr-type cto-stx))]
               [with-type-cspec (cmplr (cmplr:header cid cargs (cmplr:header:type from-type to-type))
                                       groups
                                       raw-info)]
               [from-cspec (update-spec from-type with-type-cspec)]
               [to-cspec (update-spec to-type from-cspec)]
               [builder-cspec (run-builders to-cspec)])
    ;; (printf "initial-spec-builder: info:\n" )
    ;; (pretty-print-info (cmplr-info builder-cspec))
    ;; (println builder-cspec)
    builder-cspec))
