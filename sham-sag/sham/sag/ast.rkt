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
  racket/pretty)
 "runtime.rkt")

(provide define-ast)

(begin-for-syntax
  (require racket)
  (require syntax/datum)

  (define (spec:private->public aid sid pspec formatter)

    (define (do-group s)
      (match s
        [(ast:group id parent nodes info)
         (let* ([syn-id (format-group-id formatter s)]
                [group-args (format-group-args formatter s)]
                [group-args-assoc (map (λ (n) (cons n #f)) group-args)])
           (define (do-node ns)
             (match ns
               [(ast:node id pattern info)
                (let* ([node-id (format-node-id formatter s ns)]
                       [node-args (format-node-args formatter s ns)]
                       [node-arg-type-assoc (map (λ (n) (cons n #f)) node-args)])
                  (cons (syntax->datum id)
                        (pub:ast:node id node-id node-arg-type-assoc pattern (info->hash info))))]))
           (cons (syntax->datum id)
                 (pub:ast:group id syn-id parent group-args (make-hash (map do-node nodes)) (info->hash info))))]))
    (match pspec
      [(ast gs inf)
       (pub:ast aid sid (make-hash (map do-group gs)) (info->hash inf))]))

  (define (build-syntax ast-id raw-ast-spec)
    (define temp-ast-id (generate-temporary ast-id))

    (define formatter
      (cond [(info-value (ast-info raw-ast-spec) `format) => (λ (f) ((syntax-local-value f) ast-id raw-ast-spec))]
            [else (basic-id-formatter ast-id raw-ast-spec #`: #`:)]))
    (define ast-spec (spec:private->public ast-id temp-ast-id raw-ast-spec formatter))
    (define constructor
      (cond [(info-value (pub:ast-info ast-spec) `constructor) => (λ (f) ((syntax-local-value f) ast-spec))]
            [else (rkt-ast-constructor ast-spec)]))

    (define gens (flatten
                  (map (λ (g) ((syntax-local-value g) ast-spec))
                       (info-value (ast-info raw-ast-spec) `with '()))))
    ;; list of structures producing generics/methods for groups and nodes
    (define (map-gen f) (append* (filter identity (map f gens))))
    (define (fold-gen f base) (foldr f base gens))

    (define top-struct (fold-gen (curryr build-top-struct ast-spec)
                                 (construct-top-struct constructor ast-spec)))
    (define (group-def group-spec)
      (define (node-def node-spec)
        (cons (fold-gen (curryr build-node-struct ast-spec group-spec node-spec)
                        (construct-node-struct constructor ast-spec group-spec node-spec))
              (fold-gen (curryr build-node-extra ast-spec group-spec node-spec)
                        empty)))
      (define node-defs (append-map node-def (pub:group-nodes group-spec)))

      (define struct-syntax (fold-gen (curryr build-group-struct ast-spec group-spec)
                                      (construct-group-struct constructor ast-spec group-spec)))
      (define extra-syntax (fold-gen (curryr build-group-extra ast-spec group-spec)
                                     empty))
      (append (cons struct-syntax extra-syntax) node-defs))
    (values
     (map ->syntax
          (cons top-struct
                 (append-map group-def (hash-values (pub:ast-groups ast-spec)))))
     ast-spec)))

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define-values (ast-syntaxes ast-spec) (build-syntax #`cid (attribute gs.spec)))
     (define spec-storage (pub:spec->storage ast-spec))
     (define stx
       #`(begin
           (define-syntax cid #,spec-storage)
           #,@ast-syntaxes))
     ;; (pretty-print (syntax->datum stx))
     stx]))
