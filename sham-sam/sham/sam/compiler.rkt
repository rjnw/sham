#lang racket
(require (for-syntax syntax/parse
                     racket/match
                     racket/list
                     racket/pretty
                     "syntax/spec.rkt"
                     "common/ooo.rkt"
                     "common/generics.rkt"
                     (submod "syntax/private/spec.rkt" compiler)
                     (submod "syntax/private/syntax-class.rkt" compiler)
                     (submod "generics.rkt" compiler))
         racket/stxparam)
(require (for-template (prefix-in rkt: racket)))
(provide (all-defined-out))

(define-syntax-parameter compile (make-rename-transformer #'rkt:compile))
(define-syntax-parameter ^ (make-rename-transformer #'rkt:^))

(begin-for-syntax
  (define (get-ast-spec syn)
    (define-values (spec-value ff) (syntax-local-value/immediate syn))
    (unless (ast? spec-value) (error 'sham:sam "unknown ast specification" syn))
    spec-value)

  (define (build-compiler-syntax raw-cmplr-spec)
    (define builders (cmplr-info raw-cmplr-spec)) ;TODO get from info + defaults
    (define (foldr-builders f base) (foldr f base builders))
    (define cmplr-spec (foldr-builders update-spec empty))
    (match-define (cmplr header groups info) cmplr-spec)

    (define (do-group group-spec)
      (match-define (cmplr:group name type nodes info) group-spec)
      (define (do-node node-pair)
        (match-define (cons binding bodys) node-pair)
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
        (define node-binding (parse-node-expr binding))
        (define node-bodys (parse-node-bodys bodys `(binding ,node-binding)))
        (define node-spec (cmplr:node node-binding node-bodys))
        (foldr-builders (build-node cmplr-spec group-spec node-spec) empty))
      (define node-defs (map do-node nodes))
      (define group-def (foldr-builders (build-group cmplr-spec group-spec) empty))
      (append node-defs group-def))

    (define body-def (foldr-builders (build-body cmplr-spec) empty))
    (define cmplr-stx (map ->syntax (append (append-map do-group groups) (list body-def))))
    (values cmplr-stx cmplr-spec)))

(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ cmplr:compiler-spec)
     (define-values (cmplr-stx cmplr-spec) (build-compiler-syntax (attribute cmplr.spec)))
     ;; (match-define (cmplr header groups info) cmplr-spec)
     (define stx
       #`(begin
           ;; (define-syntax #,(cmplr:header-id header) #,(compiler-syntax-storage cmplr-spec))
           #,@cmplr-stx))
     (pretty-print stx)
     #`42]))
