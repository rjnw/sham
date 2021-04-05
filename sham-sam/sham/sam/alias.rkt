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
         (match-define (cons _ (ast:node (ast:id nido nidt nidf) nargs npat ninfo)) ns)
         (list #`(define-syntax #,(format-id stx "~a~a~a" t #`sep nido) (make-rename-transformer #'#,nidf))))
       (match (lookup-group-spec spec f)
         [(ast:group (ast:id gido gidt gidf) gpid gargs gnodes ginfo)
          (list* ;; #`(define-syntax #,t (make-rename-transformer #'#,gidf))
                 (map generate-node-renames gnodes))]))
     #`(begin #,@(flatten (for/list ([f (syntax-e #`(from ...))]
                                         [t (syntax-e #`(to ...))])
                                (generate-renames f t))))]))
