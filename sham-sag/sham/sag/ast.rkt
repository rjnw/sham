#lang racket

(require
 (for-syntax "syntax/private/spec.rkt"
             "syntax/private/syntax-class.rkt"
             "syntax/private/generics.rkt"
             "syntax/private/utils.rkt"
             (prefix-in pub: "syntax/spec.rkt")
             "generics.rkt"
             syntax/parse
             racket/syntax
             racket/pretty)
 "runtime.rkt")

(provide define-ast)

(begin-for-syntax
  (require racket)
  (require syntax/datum)

  (define (spec:private->public aid ps formatter)
    (define (do-group gs)
      (match gs
        [(ast:group id parent nodes info)
         (define (do-group-args args)
           (for/list ([arg args])
             (syntax-parse arg
               [i:identifier
                (pub:ast:group:arg (pub:ast:id #`i (generate-temporary #`i) (format-group-arg-id #`i ps gs))
                                   (type-from-id #`i)
                                   '())]
               [(i:identifier ki:keyword-info)
                (pub:ast:group:arg (pub:ast:id #`i (generate-temporary #`i) (format-group-arg-id #`i ps gs))
                                   (type-from-id #`i)
                                   (attribute ki.spec))
                ;; (define if-default (info-value (attribute ki.spec) `#:default))
                ;; (define if-mutable (info-value (attribute ki.spec) `#:mutable))
                ;; #`(i #,@(if if-default (list #`#:auto) `())
                ;;      #,@(if if-mutable (list #`#:mutable) `()))
                ])))
         (let* ([syn-id (format-group-id formatter id ps gs)]
                [group-args (do-group-args (info-values info `common))])
           (define (do-node ns)
             (define (do-node-args pat (depth 0))
               (define (format-arg s) (format-node-arg-id formatter s ps gs ns))
               (define (build-type c s) ;; TODO checker c
                 (match (type-from-id s)
                   [`("!") (pub:ast:type:external (id-without-type s) depth)]
                   [t (pub:ast:type:internal t depth)]))
               (match pat
                 [(ast:pat:single c s) (list (pub:ast:node:arg (pub:ast:id s (generate-temporary s) (format-arg s))
                                                               (build-type c s) #f))]
                 [(ast:pat:datum d) (list #f)]
                 [(ast:pat:multiple s) (flatten (for/list ([p s]) (do-node-args p depth)))]
                 [(ast:pat:repeat r k) (do-node-args r (add1 depth))]))
             (match ns
               [(ast:node id pattern info)
                (let* ([node-id (format-node-id formatter id ps gs ns)]
                       [node-args (filter (compose not false?) (do-node-args pattern))])
                  (cons (syntax->datum id)
                        (pub:ast:node (cons id (generate-temporary id)) node-id
                                      node-args pattern (info->assoc info))))]))
           (cons (syntax->datum id)
                 (pub:ast:group (pub:ast:id id (generate-temporary id) syn-id)
                                parent group-args (map do-node nodes) (info->assoc info))))]))
    (match ps
      [(ast gs inf)
       (pub:ast aid (pub:ast:id aid (generate-temporary aid) aid) (map do-group gs) (info->assoc inf))]))

  (define (build-syntax ast-id raw-ast-spec)

    (define formatter
      (cond [(info-value (ast-info raw-ast-spec) `format) => (λ (f) ((syntax-local-value f) ast-id))]
            [else (basic-id-formatter ast-id #`: #`:)]))
    (define ast-spec (spec:private->public ast-id raw-ast-spec formatter))
    (match-define (cons constructor req-builders)
      (cond [(info-value (pub:ast-info ast-spec) `constructor)
             => (λ (f) ((syntax-local-value f) ast-spec))]
            [else (default-rkt-struct-constructor ast-spec)]))

    (define gens (append
                  req-builders
                  (flatten
                   (map (λ (g) ((syntax-local-value g) ast-spec))
                        (info-value (ast-info raw-ast-spec) `with '())))))
    ;; list of structures producing generics/methods for groups and nodes
    (define (map-gen f) (append* (filter identity (map f gens))))
    ;; filter-map
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
                 (append-map group-def (map cdr (pub:ast-groups ast-spec)))))
     ast-spec)))

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define-values (ast-syntaxes ast-spec) (build-syntax #`cid (attribute gs.spec)))
     (match-define (pub:ast id sid grps inf) ast-spec)
     (define spec-storage (pub:spec->storage ast-spec))
     (define stx
       #`(begin
           (define-for-syntax #,sid #,spec-storage)
           (define-syntax cid #,sid)
           #,@ast-syntaxes))
     (pretty-print (syntax->datum stx))
     stx]))


(module+ test
  (require rackunit)
  (define lam-stx
    #`(define-ast LC
        (expr
         [lambda ('lambda (n) body)
           #:type ([body : expr])
           #:identifier (n)
           #:bind [n #:in-scope body]]
         [letrec ('letrec ((ids vals) (... ...)) e)
           #:type
           ([vals : expr]
            [e : expr])
           #:identifier (ids)]
         [app (rator rand (... ...))
              #:type
              ([rator : expr]
               [rand : expr])]
         [sym !identifier]
         [num !integer])))
  (syntax->datum (expand lam-stx))
  ;; (pretty-print (syntax->datum (expand lam-stx)))
  )
