#lang racket

(require
 (for-syntax
  "private/ast-syntax-structs.rkt"
  "private/ast-syntax-class.rkt"
  "private/ast-syntax-generics.rkt"
  syntax/parse
  racket/syntax
  racket/pretty))

(provide define-ast)

(begin-for-syntax
  (require racket)
  (require syntax/datum)

  (define (build-defs ast-id raw-ast-spec)
    (match-define (ast groups info) raw-ast-spec)
    (define (build-group-map spec)
      (match spec
        [`(,groups ...)
         (map build-group-map groups)]
        [(ast:group name _ _ meta-info)
         (cons (syntax->datum name) spec)]))

    (define group-spec-map (make-hash (build-group-map groups)))
    (define ast-spec (ast group-spec-map info))

    (define formatter
      (cond [(info-value info `#:formatter) => (λ (f) ((syntax-local-value f) ast-id ast-spec))]
            [else (rkt-struct-formatter ast-id ast-spec #`: #`:)]))

    (define gens (map (λ (g) ((syntax-local-value g) ast-id ast-spec))
                      (info-value info `#:with)))
    ;; these are list of structures which produce generics/methods for groups and generic methods for nodes
    (define (map-gen f) (append* (filter identity (map f gens))))

    (define (lookup-group-spec group-id)
      (if group-id (hash-ref group-spec-map (syntax->datum group-id)) #f))
    (define group-name ast:group-id)
    (define (group-def group-spec)
      (define group-id (format-group-id formatter group-spec))
      (define group-parent (format-group-parent formatter group-spec))
      (define group-args (format-group-args formatter group-spec))
      (define group-methods (map-gen (curryr build-group-methods group-spec)))
      (define group-generics (map-gen (curryr build-group-generics group-spec)))
      (define (node-def node-spec)
        (define node-id (format-node-id formatter group-spec node-spec))
        (define node-args (format-node-args formatter group-spec node-spec))
        (define node-methods (map-gen (curryr build-node-methods group-spec node-spec)))
        #`(struct #,node-id #,group-id #,node-args #,@node-methods))
      (define node-defs (map node-def (ast:group-nodes group-spec)))
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
     (define struct-defs (build-defs #'cid ast-spec))
     ;; (pretty-display ast-spec)
     ;; (printf "struct-defs:\n")
     (parameterize ([pretty-print-columns 80])
       (pretty-print (map syntax->datum struct-defs)))
     #`(begin
         (require racket/generic)
         ;; (define cid #,(spec->storage #'cid ast-spec))
         #,@struct-defs)]))
