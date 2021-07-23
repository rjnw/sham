#lang racket

(require
 (for-syntax "syntax/ast.rkt"
             syntax/parse
             racket/match
             (submod "syntax/class.rkt" ast)
             (submod "syntax/spec.rkt" ast))
 "runtime.rkt")

(provide define-ast)

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ gs:ast-spec)
     (define-values (ast-syntaxes ast-spec) (build-syntax (attribute gs.spec)))
     (match-define (ast name tids grps inf) ast-spec)
     (define spec-storage (store-syntax ast-spec))
     (define stx
       #`(begin
           (define-for-syntax #,(get-sid tids) #,spec-storage)
           (define-syntax #,name #,(get-sid tids))
           #,@ast-syntaxes))
     ;; (pretty-print (syntax->datum stx))
     stx]))
