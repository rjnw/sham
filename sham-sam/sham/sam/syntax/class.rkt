#lang racket

(require syntax/parse)

(require "kw-info.rkt"
         "ooo.rkt")

(module* pat #f
  (require "pat.rkt"
           "ooo.rkt")
  (provide (all-defined-out))
  (define-syntax-class syn-pat
    (pattern name:id #:attr pat (pat:var #`name)))
  (define-syntax-class dat-pat
    (pattern ((~datum quote) val:id) #:attr pat (pat:dat #`val)))
  (define-splicing-syntax-class ooo-pat
    (pattern (~seq maybe-repeat:expr maybe-ooo:id)
             #:when (ooo? #`maybe-ooo)
             #:attr pat (pat:ooo (attribute maybe-repeat) (ooo #`maybe-ooo))))
  (define-splicing-syntax-class maybe-ooo-pat
    (pattern o:ooo-pat #:attr pat (attribute o.pat))
    (pattern v:expr #:attr pat #`v))
  (define-syntax-class seq-pat
    (pattern (p:maybe-ooo-pat ...)
             #:attr pat (pat:seq (apply vector-immutable (attribute p.pat)))))
  (define-syntax-class app-pat
    (pattern (v:expr . body:expr) #:attr pat (pat:app #`v #`body)))

  (define-syntax-class any-pat
    (pattern s:syn-pat #:attr pat (attribute s.pat))
    (pattern s:dat-pat #:attr pat (attribute s.pat))
    (pattern s:seq-pat #:attr pat (attribute s.pat))
    (pattern s:app-pat #:attr pat (attribute s.pat))))

(module* ast #f
  (provide (all-defined-out))
  (require (submod "spec.rkt" ast))
  (define-syntax-class node-pattern
    (pattern name:id
             #:attr spec (ast:pat:single #`name #f))
    (pattern ((~datum quote) datum:id)
             #:attr spec (ast:pat:datum #`datum))
    (pattern ((~datum ?) check:id name:id)
             #:attr spec (ast:pat:single #`name (cons '? #`check)))
    (pattern ((~datum ~) type:expr name:id)
             #:attr spec (ast:pat:single #`name (cons '~ #`type)))
    (pattern ((~datum !) type:expr name:id)
             #:attr spec (ast:pat:single #`name (cons '! #`type)))
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
             #:attr spec (ast:node #`var (attribute info.spec) #f (attribute def.spec)))
    (pattern (var:id info:keyword-info)
             #:attr spec (ast:node #`var (attribute info.spec) #f (ast:pat:datum #`var))))

  (define-syntax-class ast-group
    #:description "ast group specification"
    (pattern (name:id (~optional parent:id) nodes:ast-node ... info:keyword-info)
             #:attr spec (ast:group #`name (attribute info.spec) (attribute parent) #f (attribute nodes.spec))))

  (define-splicing-syntax-class ast-spec
    (pattern (~seq name:id groups:ast-group ... info:keyword-info)
             #:attr spec (ast #f #`name (attribute groups.spec) (attribute info.spec)))))

(module* reader #f
  (require (submod "spec.rkt" reader))
  (provide (all-defined-out))
  (define-syntax-class language-spec
    #:description "language specification"
    (pattern (lang:id (name:id var:id ...) ...)))
  (define-splicing-syntax-class reader-spec
    #:description "sham language reader specification"
    (pattern (~seq (read-id read-syntax-id) ast:id info:keyword-info)
             #:attr spec (reader (cons #`read-id #`read-syntax-id) #`ast (attribute info.spec)))))

(module* compiler #f
  (require (submod "spec.rkt" compiler))
  (provide (all-defined-out))

  (define-syntax-class compiler-type
    (pattern (from:expr (~datum ->) to:expr) #:attr spec (cmplr:header:type #'from #'to)))

  (define-syntax-class compiler-group
    (pattern (name:id type:compiler-type
                      (node-binding:expr node-dirs ... node-body:expr) ...
                      info:keyword-info)
             #:attr spec (cmplr:group #`name
                                      (attribute type.spec)
                                      (map cmplr:node
                                           (attribute node-binding)
                                           (attribute node-dirs)
                                           (attribute node-body))
                                      (attribute info.spec))))

  (define-splicing-syntax-class compiler-header
    (pattern (~seq (name:id [arg:id dflt:expr] ...) type:compiler-type)
             #:attr spec (cmplr:header #`name (map cons (attribute arg) (attribute dflt)) (attribute type.spec))))

  (define-splicing-syntax-class compiler-spec
    (pattern (~seq header:compiler-header groups:compiler-group ... info:keyword-info)
             #:attr spec (cmplr (attribute header.spec) (attribute groups.spec) (attribute info.spec)))))
