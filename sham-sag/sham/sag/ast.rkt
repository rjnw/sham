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

  (define (spec:private->public aid sid ps formatter)
    (define (do-group gs)
      (match gs
        [(ast:group id parent nodes info)
         (define (do-group-args args)
           (for/list ([arg args])
             (syntax-parse arg
               [i:identifier
                (pub:ast:group:arg (cons #`i (generate-temporary #`i))
                                   (format-group-arg-id #`i ps gs)
                                   (type-from-id #`i)
                                   '())]
               [(i:identifier ki:keyword-info)
                (pub:ast:group:arg (cons #`i (generate-temporary #`i))
                                   (format-group-arg-id #`i ps gs)
                                   (type-from-id #`i)
                                   (attribute ki.spec))
                ;; (define if-default (info-value (attribute ki.spec) `#:default))
                ;; (define if-mutable (info-value (attribute ki.spec) `#:mutable))
                ;; #`(i #,@(if if-default (list #`#:auto) `())
                ;;      #,@(if if-mutable (list #`#:mutable) `()))
                ])))
         (let* ([syn-id (format-group-id formatter id ps gs)]
                [group-args (do-group-args (info-values info `#:common))])
           (define (do-node ns)
             (define (do-node-args pat (depth 0))
               (define (format-arg s) (format-node-arg-id formatter s ps gs ns))
               (define (build-type s)
                 (match (type-from-id s)
                   [`("!") (pub:ast:type:external (id-without-type s) depth)]
                   [t (pub:ast:type:internal t depth)]))
               (match pat
                 [(ast:pat:single s) (list (pub:ast:node:arg s (format-arg s) (build-type s) #f))]
                 [(ast:pat:datum d) (list #f)]
                 [(ast:pat:checker c s) (list (pub:ast:node:arg s (format-arg s) (pub:ast:type:external c depth) #f))]
                 [(ast:pat:multiple s) (append-map (curryr do-node-args depth) s)]
                 [(ast:pat:repeat r) (do-node-args r (add1 depth))]))
             (match ns
               [(ast:node id pattern info)
                (let* ([node-id (format-node-id formatter id ps gs ns)]
                       [node-args (filter (compose not false?) (do-node-args pattern))])
                  (cons (syntax->datum id)
                        (pub:ast:node (cons id (generate-temporary id)) node-id
                                      node-args pattern (info->hash info))))]))
           (cons (syntax->datum id)
                 (pub:ast:group (cons id (generate-temporary id)) syn-id
                                parent group-args (make-hash (map do-node nodes)) (info->hash info))))]))
    (match ps
      [(ast gs inf)
       (pub:ast aid sid (make-hash (map do-group gs)) (info->hash inf))]))

  (define (build-syntax ast-id raw-ast-spec)
    (define temp-ast-id (generate-temporary ast-id))

    (define formatter
      (cond [(info-value (ast-info raw-ast-spec) `#:format) => (λ (f) ((syntax-local-value f) ast-id))]
            [else (basic-id-formatter ast-id #`: #`:)]))
    (define ast-spec (spec:private->public ast-id temp-ast-id raw-ast-spec formatter))
    (define constructor
      (cond [(info-value (pub:ast-info ast-spec) `constructor) => (λ (f) ((syntax-local-value f) ast-spec))]
            [else (rkt-ast-struct-constructor ast-spec)]))

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
     (pretty-print (syntax->datum stx))
     stx]))
