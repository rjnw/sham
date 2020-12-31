#lang racket

(require syntax/parse racket/syntax)
(provide ast-spec language-spec
         keyword-info)

(require "spec.rkt")

(define-splicing-syntax-class keyword-value
  (pattern (~seq k:keyword v:expr ...)
           #:attr spec (cons (string->symbol (keyword->string (syntax->datum #`k)))
                             (syntax->list #`(v ...)))))
(define-splicing-syntax-class keyword-info
  (pattern (~seq ki:keyword-value ...)
           #:attr spec (attribute ki.spec)))

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
  #:description "node production production"
  (pattern (var:id def:node-pattern info:keyword-info)
           #:attr spec (ast:node #`var (attribute def.spec) (attribute info.spec))))

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
