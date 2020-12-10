#lang racket

(require
 (for-syntax
  "private/spec.rkt"
  "private/syntax-class.rkt"
  "private/generics.rkt"
  "private/utils.rkt"
  (prefix-in pub: "spec.rkt")
  syntax/parse
  racket/syntax
  racket/pretty))

(provide define-ast)

(begin-for-syntax
  (require racket)
  (require syntax/datum)

  (define (spec:private->public aid pspec formatter)
    (define (build-group-assoc spec)
      (match spec
        [`(,groups ...)
         (map build-group-map groups)]
        [(pub:ast:group name syn-id parent cargs nodes meta-info)
         (cons (syntax->datum name) spec)]))

    (define (rec s)
      (match s
        [(ast gs inf) (pub:ast aid (make-hash (build-group-assoc (map rec gs))) info)]
        [(ast:group id parent nodes info)
         (let* ([syn-id (format-group-id formatter group-spec)]
                [group-args (format-group-args formatter group-spec)])
           (pub:ast:group id syn-id group-parent group-args (map rec nodes) info))]
        [(ast:node id pattern info)
         (let* ([node-id (format-node-id formatter group-spec node-spec)]
                [node-args (format-node-args formatter group-spec node-spec)])
           (pub:ast:node id node-args pattern info))]))
    (rec pspec))

  (define (build-syntax ast-id raw-ast-spec)
    ;; (define group-spec-map (make-hash (build-group-map groups)))
    ;; (define ast-spec (ast group-spec-map info))

    (define formatter
      (cond [(info-value info `#:format) => (λ (f) ((syntax-local-value f) ast-id ast-spec))]
            [else (rkt-struct-formatter ast-id raw-ast-spec #`: #`:)]))
    (define ast-spec (spec:private->public raw-ast-spec formatter))

    (define gens (flatten
                  (map (λ (g) ((syntax-local-value g) ast-spec))
                       (info-value info `#:with))))
    ;; list of structures producing generics/methods for groups and nodes
    (define (map-gen f) (append* (filter identity (map f gens))))

    (define (group-def group-spec)
      (match-define (pub:ast:group id syn-id parent common-args nodes info) group-spec)

      (define group-methods (map-gen (curryr build-group-methods formatter group-spec)))
      (define group-generics (map-gen (curryr build-group-generics formatter group-spec)))

      (define (node-def node-spec)
        (define node-id (format-node-id formatter group-spec node-spec))
        (define node-args (format-node-args formatter group-spec node-spec))

        (define struct-syntax
          (for/fold [(syn #`(struct #,node-id #,group-id #,node-args))]
                    [(g gens)]
            (build-node-struct g syn group-spec node-spec)))
        (define extra-syntax (map-gen (curryr build-node-extra group-spec node-spec)))
        (cons struct-syntax extra-syntax))

      (define node-defs (append-map node-def (ast:group-nodes group-spec)))

      (append
       (list*
        #`(struct #,group-id #,@(if group-parent (list group-parent) (list))
            #,group-args
            #,@group-methods)
        group-generics)
       node-defs))
    (define group-defs (flatten (map group-def groups)))
    group-defs))

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define ast-spec (attribute gs.spec))
     (define struct-syntax (build-syntax #`cid ast-spec))
     (define storage (spec->storage #`cid ast-spec))
     ;; (parameterize ([pretty-print-columns 80])
     ;;   (pretty-display ast-spec)
     ;;   (pretty-print (map syntax->datum struct-defs))
     ;;   (pretty-print (syntax->datum storage)))
     #`(begin
         (require racket/generic)
         (define-syntax cid #,storage)
         #,@struct-syntax)]))
