#lang racket

(require (for-template racket/match))

(provide (all-defined-out))

(struct term-type [transformer match-expander self-spec top-spec]
  #:property prop:procedure (lambda (tt stx) ((term-type-transformer tt) tt stx))
  #:property prop:match-expander (struct-field-index match-expander))
