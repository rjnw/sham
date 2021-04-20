#lang racket

(require (for-syntax "syntax/spec.rkt"
                     racket/match
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide define-alias)

(require (for-syntax racket/pretty))
(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ #:for tid:id
        (~optional (~seq #:sep sep:id) #:defaults ([sep #'||]))
        [from:id (~datum ->) (~optional to:id #:defaults ([to #'||]))] ...)
     (define-values (spec _) (syntax-local-value/immediate #`tid))
     (define (generate-renames f t)
       (define (generate-node-renames ns)
         (match-define (cons _ (ast:node nids ninfo nargs npat)) ns)
         (list #`(define-syntax #,(format-id stx "~a~a~a" t #`sep (get-oid nids))
                   (make-rename-transformer #'#,(get-oid nids)))))
       (match (find-group-spec f spec)
         [(ast:group gids ginfo gpid gargs gnodes)
          (list* ;; #`(define-syntax #,t (make-rename-transformer #'#,gidf))
                 (map generate-node-renames gnodes))]))
     #`(begin #,@(flatten (for/list ([f (syntax-e #`(from ...))]
                                         [t (syntax-e #`(to ...))])
                                (generate-renames f t))))]))
