#lang racket

(require syntax/parse racket/syntax)
(provide ast-spec language-spec)

(require "ast-syntax-structs.rkt")

(define-splicing-syntax-class keyword-value
  (pattern (~seq k:keyword v:expr ...)))
(define-splicing-syntax-class keyword-info
  (pattern (~seq ki:keyword-value ...)))

(define-syntax-class contract
  (pattern name:id #:attr spec this-syntax)
  (pattern (names:id ...) #:attr spec (syntax->list this-syntax)))
(define-syntax-class node-contract
  (pattern (name:id (~datum :) c:contract) #:attr spec (ast:node-contract (attribute c.spec))))
(define-splicing-syntax-class node-contracts
  (pattern (~seq nc:node-contract ...) #:attr spec (attribute nc.spec)))

(define-syntax-class node-pattern
  (pattern name:id
           #:attr spec (ast:pat:single #`name))
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
           #:attr spec (ast:node #`var (attribute def.spec) #`info))
  (pattern (var:id (~datum #:terminal) proc:id)
           #:attr spec (ast:term-node #`var #`proc)))

(define-syntax-class ast-group
  #:description "ast group specification"
  (pattern (name:id (~optional parent:id) nodes:ast-node ... info:keyword-info)
           #:attr spec (ast:group #`name (attribute parent) (attribute nodes.spec) #`info)))

(define-splicing-syntax-class ast-spec
  (pattern (~seq groups:ast-group ... info:keyword-info)
           #:attr spec (ast (attribute groups.spec) #`info)))

(define-syntax-class language-spec
  #:description "language specification"
  (pattern (lang:id (name:id var:id ...) ...)))

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
