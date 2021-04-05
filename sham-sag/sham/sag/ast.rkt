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
                                   (attribute ki.spec))])))
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
                        (pub:ast:node (pub:ast:id id (generate-temporary id) node-id)
                                      node-args pattern (info->assoc info))))]))
           (cons (syntax->datum id)
                 (pub:ast:group (pub:ast:id id (generate-temporary id) syn-id)
                                parent group-args (map do-node nodes) (info->assoc info))))]))
    (match ps
      [(ast gs inf)
       (pub:ast aid (pub:ast:id aid (generate-temporary aid) aid) (map do-group gs) (info->assoc inf))]))

  (define (build-syntax ast-id raw-ast-spec)
    (define formatter
      (let ([fv (car (info-value (ast-info raw-ast-spec) `format))])
        (cond [(and (identifier? fv) (syntax-local-value fv #f)) => (λ (f) (f))]
              [else (get-formatter ast-id (syntax->datum fv))])))
    (define default-builders
      (cond [(info-value (ast-info raw-ast-spec) `default)
             => (λ (f) ((syntax-local-value f)))]
            [else (default-rkt-struct-builder)]))
    (define raw-builders
      (append
       (flatten
        (map (λ (g) ((syntax-local-value g)))
             (info-value (ast-info raw-ast-spec) `with '())))
       default-builders))

    (define all-builders (foldr update-others raw-builders raw-builders))
    (define (foldr-builders f base) (foldr f base all-builders))

    (define ast-spec (foldr-builders build-spec (spec:private->public ast-id raw-ast-spec formatter)))

    (define top-struct (foldr-builders (curryr build-top ast-spec) empty))
    (define (group-def group-spec)
      (define (node-def node-spec) (foldr-builders (curryr build-node ast-spec group-spec node-spec) empty))
      (define node-defs (append-map node-def (pub:group-nodes group-spec)))
      (define struct-defs (foldr-builders (curryr build-group ast-spec group-spec) empty))
      (append struct-defs node-defs))

    (values
     (map ->syntax
          (append top-struct
                  (append-map (compose group-def cdr) (pub:ast-groups ast-spec))))
     ast-spec)))

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define-values (ast-syntaxes ast-spec) (build-syntax #`cid (attribute gs.spec)))
     (match-define (pub:ast id (pub:ast:id oid gid fid) grps inf) ast-spec)
     (define spec-storage (pub:spec->storage ast-spec))
     (define stx
       #`(begin
           (define-for-syntax #,gid #,spec-storage)
           (define-syntax cid #,gid)
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
