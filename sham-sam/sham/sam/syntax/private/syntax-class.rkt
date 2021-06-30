#lang racket

(require syntax/parse racket/syntax)
(provide ast-spec language-spec
         keyword-info)

(require "spec.rkt"
         "utils.rkt")

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
  (pattern ((~datum ?) check:id name:id)
           #:attr spec (ast:pat:single (cons '? #`check) #`name))
  (pattern ((~datum ~) type:expr name:id)
           #:attr spec (ast:pat:single (cons '~ #`type) #`name))
  (pattern ((~datum !) type:expr name:id)
           #:attr spec (ast:pat:single (cons '! #`type) #`name))
  (pattern (multiple:node-multiple-pattern ...)
           #:attr spec (ast:pat:multiple (apply vector-immutable (attribute multiple.spec)))))
(define-splicing-syntax-class node-multiple-pattern
  ;; (pattern (~seq repeat:node-pattern (~datum ...))
  ;;          #:attr spec (ast:pat:repeat (attribute repeat.spec) (cons 0 #f)))
  (pattern (~seq maybe-repeat:node-pattern maybe-ooo:id)
           #:when (ooo? #`maybe-ooo)
           #:attr spec (ast:pat:repeat (attribute maybe-repeat.spec) (ooo #`maybe-ooo)))
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

(module* reader #f
  (require (submod "spec.rkt" reader))
  (provide (all-defined-out))
  (define-splicing-syntax-class reader-spec
    #:description "sham language reader specification"
    (pattern (~seq ast:id info:keyword-info)
             #:attr spec (reader #`ast (attribute info.spec)))))

(module* compiler #f
  (require (submod "spec.rkt" compiler))
  (provide (all-defined-out))

  (define-syntax-class compiler-type
    (pattern (from:id (~datum ->) to:id) #:attr spec (cmplr:type #'from #'to)))

  (define-splicing-syntax-class (compiler-pattern #:legal-ops legal-ops)
    (pattern (~seq (~var maybe-repeat (compiler-pattern #:legal-ops legal-ops)) maybe-ooo:id)
             #:when (ooo? #`maybe-ooo)
             #:attr spec (cmplr:pat:ooo (attribute maybe-repeat.spec) (ooo #`maybe-ooo)))
    (pattern (op:id body:expr ...)
             #:when (ormap (λ (f) (f #`op)) legal-ops)
             #:attr spec (cmplr:pat:op #`op (attribute body)))
    (pattern (rator:id (~var rands (compiler-pattern #:legal-ops legal-ops)) ...)
             #:attr spec (cmplr:pat:app #`rator (attribute rands.spec)))
    (pattern var:id #:attr spec #`var))

  (define-syntax-class (compiler-node #:binding-ops binding-ops #:body-ops body-ops)
    (pattern ((~var bpat (compiler-pattern #:legal-ops binding-ops))
              (~var body (compiler-pattern #:legal-ops body-ops)))
             #:attr spec (cons (attribute bpat.spec) (attribute body.spec))))
  (define-syntax-class (compiler-group #:legal-ops legal-ops)
    ;; each group is a separate recursive function performing over set of production nodes
    (pattern (name:id type:compiler-type
                      info:keyword-info
                      (~var node (compiler-node
                                  #:binding-ops (car legal-ops)
                                  #:body-ops (cdr legal-ops))) ...)
             #:attr spec (cmplr:group #'name (attribute type.spec) (attribute node.spec) (attribute info.spec))))

  (define-splicing-syntax-class compiler-header
    (pattern (~seq (name:id [arg:id dflt:expr] ...) type:compiler-type)
             #:attr spec (cmplr:header #`name (map cons (attribute arg) (attribute dflt)) (attribute type.spec))))

  (define-splicing-syntax-class (compiler-spec #:legal-ops legal-ops)
    (pattern (~seq header:compiler-header (~var groups (compiler-group #:legal-ops legal-ops)) ...)
             #:attr spec (cmplr (attribute header.spec) (attribute groups.spec)))))
