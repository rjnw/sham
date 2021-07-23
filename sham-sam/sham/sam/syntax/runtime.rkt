#lang racket

(require (for-template racket/match))

(provide (all-defined-out))

(struct stxt [transformer]
  #:property prop:procedure (lambda (tt stx) ((stxt-transformer tt) tt stx)))

(module* ast #f
  (provide (all-defined-out))
  (struct term-type stxt [match-expander self-spec top-spec]
    #:property prop:procedure (lambda (tt stx) ((stxt-transformer tt) tt stx))
    #:property prop:match-expander (struct-field-index match-expander)))

(module* compiler #f
  (provide (all-defined-out))
  (struct sam-compiler stxt []))
