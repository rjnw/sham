#lang racket

(require (for-syntax "syntax/spec.rkt"
                     racket/match
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide define-aliases)

(require (for-syntax racket/pretty))
(define-syntax (define-aliases stx)
  (syntax-parse stx
    [(_ tid:id
        (~optional (~seq #:sep sep:id) #:defaults ([sep #'||]))
        [from:id (~optional (~seq (~datum ->) to:id))] ...)
     (define-values (spec _) (syntax-local-value/immediate #`tid))
     (define (generate-renames f t)
       (define (generate-node-renames ns)
         (match-define (cons _ (ast:node nids ninfo nargs npat)) ns)
         (list #`(define-syntax #,(format-id stx "~a~a~a" t #`sep (get-oid nids))
                   (make-rename-transformer #'#,(get-fid nids)))))
       (match (find-group-spec f spec)
         [(ast:group gids ginfo gpid gargs gnodes)
          (list* ;; #`(define-syntax #,t (make-rename-transformer #'#,gidf))
                 (map generate-node-renames gnodes))]))
     #`(begin #,@(flatten (for/list ([f (syntax-e #`(from ...))]
                                     [t (syntax-e #`((~? to from) ...))])
                            (generate-renames f t))))]))
