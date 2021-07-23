#lang racket

(require
 "syntax/spec.rkt"
 (submod "syntax/private/spec.rkt" compiler)
 (submod "syntax/private/syntax-class.rkt" compiler))

(define-generics compiler-pattern
  (expand-pattern stx input-zipper)
  (perform-pattern stx output-zipper)
  )

(define (get-ast-spec syn)
  (define-values (spec-value ff) (syntax-local-value/immediate syn))
  (unless (ast? spec-value) (error 'sham:sam "unknown ast specification ~a:~a" syn spec-value))
  spec-value)

(define (build-compiler-syntax raw-cmplr-spec)
  (define builders (cmplr-info raw-cmplr-spec)) ;TODO get from info + defaults
  (define (foldr-builders f base) (foldr f base builders))
  (define cmplr-spec (foldr-builders update-spec empty))
  (match-define (cmplr header groups info) cmplr-spec)

  (define (do-group group-spec)
    (match-define (cmplr:group name type nodes info) group-spec)
    (define (do-node node-spec)
      (match-define (cmplr:node binding-pattern bodys) node-spec)
      (define (is-legal-op op) void)
      (define ((apply-pattern op) rands) void)
      (define (parse-node-expr e p)
        (let rec ([stx e]
                  [path p])
          (match (parse-for-ooo stx)
            [(cons op rands)
             (cond [(is-legal-op op) => (apply-pattern rands)]
                   [else (cmplr:pat:app op (map (λ (r) (rec r `(app ,op ,rands ,path))) rands))])]
            [(stx:ooo ns cnt) (cmplr:pat:ooo (rec ns `(ooo ,ns ,cnt ,path)) cnt)]
            [e e])))
      (define (parse-node-bodys bs binding-path)
        (for/fold ([out '()])
                  ([be bs] [i (length bs)])
          (parse-node-expr be `(body ,i ,be ,out ,binding-path))))

      (define node-bindings (parse-node-expr binding-pattern))
      (define node-bodys (parse-node-bodys bodys `(binding ,node-bindings)))
      (define parsed-node-spec (cmplr:node node-binding node-bodys))

      (foldr-builders (build-node cmplr-spec group-spec parsed-node-spec) empty))

    (define node-defs (map do-node nodes))
    (define group-def (foldr-builders (build-group cmplr-spec group-spec) empty))

    (append node-defs group-def))

  (define body-def (foldr-builders (build-body cmplr-spec) empty))
  (define cmplr-stx (map ->syntax (append (append-map do-group groups) (list body-def))))
  (values cmplr-stx cmplr-spec))
