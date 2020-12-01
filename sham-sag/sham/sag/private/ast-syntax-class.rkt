#lang racket

(require syntax/parse racket/syntax)
(provide ast-spec language-spec info-values info-value)

(require "ast-syntax-structs.rkt")

(define-splicing-syntax-class keyword-value
  (pattern (~seq k:keyword v:expr ...)
           #:attr spec (cons #`k #`(v ...))))
(define-splicing-syntax-class keyword-info
  (pattern (~seq ki:keyword-value ...)
           #:attr spec (attribute ki.spec)))

(define-syntax-class contract
  (pattern name:id #:attr spec this-syntax)
  (pattern (names:id ...) #:attr spec (syntax->list this-syntax)))
(define-syntax-class node-contract
  (pattern (name:id (~datum :) c:contract) #:attr spec (ast:node-contract (attribute c.spec))))
(define-splicing-syntax-class node-contracts
  (pattern (~seq nc:node-contract ...) #:attr spec (attribute nc.spec)))

(define-syntax-class node-pattern
  (pattern name:id
           #:attr spec (ast:pat:single #f #`name))
  (pattern ((~datum quote) datum:id)
           #:attr spec (ast:pat:datum #`datum))
  (pattern ((~datum ?) check:id)
           #:attr spec (ast:pat:checker #`check (generate-temporary)))
  (pattern (multiple:node-multiple-pattern ...)
           #:attr spec (ast:pat:multiple (attribute multiple.spec))))
(define-splicing-syntax-class node-multiple-pattern
  (pattern (~seq repeat:node-pattern (~datum ...))
           #:attr spec (ast:pat:repeat (attribute repeat.spec)))
  (pattern ms:node-pattern
           #:attr spec (attribute ms.spec)))

(define-syntax-class ast-node
  #:description "single production"
  (pattern (var:id def:node-pattern defc:node-contracts info:keyword-info)
           #:attr spec (ast:node:pat #`var (attribute def.spec) (attribute info.spec)))
  (pattern (var:id (~datum #:terminal) proc:id)
           #:attr spec (ast:node:term #`var #`proc)))

(define-syntax-class ast-group
  #:description "ast group specification"
  (pattern (name:id (~optional parent:id) nodes:ast-node ... info:keyword-info)
           #:attr spec (ast:group #`name (attribute parent) (attribute nodes.spec) (attribute info.spec))))

(define-splicing-syntax-class ast-spec
  (pattern (~seq groups:ast-group ... info:keyword-info)
           #:attr spec (ast (attribute groups.spec) (attribute info.spec))))

(define-syntax-class language-spec
  #:description "language specification"
  (pattern (lang:id (name:id var:id ...) ...)))

(define (info-values infos key)
  (map syntax->list
       (map cdr
            (filter (λ (kvp) (equal? (syntax->datum (car kvp)) key))
                    infos))))
(define (info-value infos key)
  (define vs (info-values infos key))
  (cond
    [(cons? vs) (car vs)]
    [(empty? vs) #f]))

(module+ test
  (require syntax/keyword)
  (define-values (opts rst)
    (syntax-parse #'(#:prefix '- #:custom-write #t)
      [(i:keyword-info)
       (parse-keyword-options #`i
                              `((#:prefix ,check-expression)
                                (#:custom-write ,check-expression)))]))
  (printf "opts: ~a, rst: ~a\n" opts rst)
  (printf "prefix: ~a\n" (options-select opts '#:prefix)))
