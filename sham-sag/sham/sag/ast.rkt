#lang racket

(require
 (for-syntax
  "private/spec.rkt"
  "private/syntax-class.rkt"
  "private/generics.rkt"
  "private/utils.rkt"
  (prefix-in pub: "spec.rkt")
  "generics.rkt"
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
         (map build-group-assoc groups)]
        [(pub:ast:group name syn-id parent cargs nodes meta-info)
         (cons (syntax->datum name) spec)]))

    (define (rec s)
      (match s
        [(ast gs inf) (pub:ast aid (make-hash (build-group-assoc (map rec gs))) inf)]
        [(ast:group id parent nodes info)
         (let* ([syn-id (format-group-id formatter s)]
                [group-args (format-group-args formatter s)]
                [group-args-assoc (map (λ (n) (cons n #f)) group-args)])
           (define (rec-node ns)
             (match ns
               [(ast:node id pattern info)
                (let* ([node-id (format-node-id formatter s ns)]
                       [node-args (format-node-args formatter s ns)]
                       [node-arg-type-assoc (map (λ (n) (cons n #f)) node-args)])
                  (pub:ast:node id node-id node-arg-type-assoc pattern info))]))
           (pub:ast:group id syn-id parent group-args (map rec-node nodes) info))]))
    (rec pspec))

  (define (build-syntax ast-id raw-ast-spec)

    (define formatter
      (cond [(info-value (ast-info raw-ast-spec) `#:format) => (λ (f) ((syntax-local-value f) ast-id ast-spec))]
            [else (rkt-struct-formatter ast-id raw-ast-spec #`: #`:)]))
    (define ast-spec (spec:private->public ast-id raw-ast-spec formatter))

    (define gens (flatten
                  (map (λ (g) ((syntax-local-value g) ast-spec))
                       (info-value (ast-info raw-ast-spec) `#:with))))
    ;; list of structures producing generics/methods for groups and nodes
    (define (map-gen f) (append* (filter identity (map f gens))))

    (define (group-def group-spec)
      (match-define (pub:ast:group gid gsyn-id parent gargs-assoc nodes info) group-spec)

      (define (node-def node-spec)
        (match-define (pub:ast:node nid nsyn-id nargs-assoc pat info) node-spec)

        (define struct-syntax
          (for/fold [(syn #`(struct #,nsyn-id #,gsyn-id #,(map car nargs-assoc)))]
                    [(g gens)]
            (build-node-struct g syn group-spec node-spec)))
        (define extra-syntax (map-gen (curryr build-node-extra group-spec node-spec)))
        (cons struct-syntax extra-syntax))
      (define node-defs (append-map node-def nodes))

      (define struct-syntax
        (for/fold [(syn #`(struct #,gsyn-id #,@(if parent (list parent) (list))
                            #,(map car gargs-assoc)))]
                  [(g gens)]
          (build-group-struct g syn group-spec)))
      (define extra-syntax (map-gen (curryr build-group-extra group-spec)))
      (append (cons struct-syntax extra-syntax) node-defs))
    (append-map group-def (hash-values (pub:ast-groups ast-spec)))))

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define ast-spec (attribute gs.spec))
     (define struct-syntax (build-syntax #`cid ast-spec))
     (define storage (spec->storage #`cid ast-spec))
     (parameterize ([pretty-print-columns 80])
       (pretty-display ast-spec)
       (pretty-print (map syntax->datum struct-syntax))
       (pretty-print (syntax->datum storage)))
     #`(begin
         (define-syntax cid #,storage)
         #,@struct-syntax)]))
