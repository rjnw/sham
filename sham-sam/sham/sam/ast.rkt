#lang racket

(require
 (for-syntax "syntax/private/spec.rkt"
             "syntax/private/syntax-class.rkt"
             "syntax/private/utils.rkt"
             "syntax/format.rkt"
             (prefix-in pub: "syntax/spec.rkt")
             "syntax/type.rkt"
             "generics.rkt"
             syntax/parse
             racket/syntax
             racket/pretty)
 "runtime.rkt")

(provide define-ast)

(begin-for-syntax
  (require racket)
  (require syntax/datum)

  (define (build-syntax ast-id raw-ast-spec)
    (define formatter (ast-format (info-value `format (ast-info raw-ast-spec))))
    (define default-builders
      (cond [(info-value `default (ast-info raw-ast-spec)) => (λ (f) ((syntax-local-value f)))]
            [else (default-rkt-struct-builder)]))
    (define raw-builders
      (append (flatten (map (λ (g) ((syntax-local-value g))) (assoc-default `with (ast-info raw-ast-spec) '())))
              default-builders))

    (define all-builders (foldr update-others raw-builders raw-builders))
    (define (foldr-builders f base) (foldr f base all-builders))

    (define ast-spec (foldr-builders update-spec (pub:from-private ast-id raw-ast-spec formatter)))

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
     (match-define (pub:ast id tids grps inf) ast-spec)
     (define spec-storage (pub:store-syntax ast-spec))
     (define stx
       #`(begin
           (define-for-syntax #,(pub:get-sid tids) #,spec-storage)
           (define-syntax cid #,(pub:get-sid tids))
           #,@ast-syntaxes))
     ;; (pretty-print (syntax->datum stx))
     stx]))
