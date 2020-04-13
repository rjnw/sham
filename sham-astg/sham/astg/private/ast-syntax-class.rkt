#lang racket

(require syntax/parse)
(provide ast-spec language-spec)

(require "ast-syntax-structs.rkt")

(define-syntax-class node-pattern
  (pattern single:id
           #:attr spec (ast:pat:single #'single))
  (pattern ((~datum quote) datum:id)
           #:attr spec (ast:pat:datum #'datum))
  (pattern ((~datum ?) check:id)
           #:attr spec (ast:pat:checker #'check #`#,(gensym 'v)))
  (pattern (multiple:node-multiple-pattern ...)
           #:attr spec (ast:pat:multiple (attribute multiple.spec))))
(define-splicing-syntax-class node-multiple-pattern
  (pattern (~seq repeat:node-pattern (~datum ...))
           #:attr spec (ast:pat:repeat (attribute repeat.spec)))
  (pattern ms:node-pattern
           #:attr spec (attribute ms.spec)))

(define-splicing-syntax-class node-info
  (pattern (~seq (~datum #:attr) v:expr)
           #:attr as (cons 'attr #'v))
  (pattern (~seq (~datum #:mutable) v:id)
           #:attr as (cons 'mutable #'v))
  (pattern (~seq (~datum #:extra) v:expr)
           #:attr as (cons 'extra #'v)))
(define-syntax-class ast-node
  (pattern (var:id def:node-pattern info:node-info ...)
           #:attr spec (ast:node #'var (attribute def.spec) (attribute info.as)))
  (pattern (var:id (~datum #:terminal) proc:id)
           #:attr spec (ast:term-node #'var #'proc)))

(define-splicing-syntax-class group-info
  (pattern (~seq (~datum #:common) v:expr)
           #:attr as (cons '(common) #'v))
  (pattern (~seq (~datum #:common-mutable) v:expr)
           #:attr as (cons '(common mutable) #'v))
  (pattern (~seq (~datum #:common-auto) v:expr)
           #:attr as (cons '(common auto) #'v))
  (pattern (~seq (~datum #:common-auto-mutable) v:expr)
           #:attr as (cons '(common auto mutable) #'v))
  (pattern (~seq (~datum #:terminals) (nodes:ast-node ...))
           #:attr as (cons '(terminals) (attribute nodes.spec))))
(define-syntax-class ast-group
  #:description "ast group specification"
  (pattern (name:id (~optional parent:id) nodes:ast-node ... meta:group-info ...)
           #:attr spec (ast:group #'name (attribute parent) (attribute nodes.spec) (attribute meta.as))))

(define-splicing-syntax-class ast-info
  (pattern (~seq (~datum #:prefix) v:expr)
           #:attr as (cons 'prefix #'v))
  (pattern (~seq (~datum #:seperator) v:expr)
           #:attr as (cons 'seperator #'v))
  (pattern (~seq (~datum #:top-seperator) v:expr)
           #:attr as (cons 'top-seperator #'v))
  (pattern (~seq (~datum #:macro-prefix) v:expr)
           #:attr as (cons 'macro-prefix #'v))
  (pattern (~seq (~datum #:macro-suffix) v:expr)
           #:attr as (cons 'macro-suffix #'v))
  (pattern (~seq (~datum #:build-macros) (~datum #f))
           #:attr as (cons 'build-macros #f))
  (pattern (~seq (~datum #:build-macros) (~datum #t))
           #:attr as (cons 'build-macros #t))
  (pattern (~seq (~datum #:build-map) (~datum #f))
           #:attr as (cons 'build-map #f))
  (pattern (~seq (~datum #:build-map) (~datum #t))
           #:attr as (cons 'build-map #t))
  (pattern (~seq (~datum #:custom-write) (~datum #f))
           #:attr as (cons 'custom-write #f))
  (pattern (~seq (~datum #:custom-write) (~datum #t))
           #:attr as (cons 'custom-write #t)))
(define-splicing-syntax-class ast-spec
  (pattern (~seq meta:ast-info ... groups:ast-group ...)
           #:attr spec (cons (attribute meta.as) (attribute groups.spec))))

(define-syntax-class language-spec
  #:description "language specification"
  (pattern (lang:id (name:id var:id ...) ...)))
