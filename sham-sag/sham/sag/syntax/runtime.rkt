#lang racket

(require (for-template racket/match))

(provide (all-defined-out))

(struct term-type [rename-transformer match-expander self-spec top-spec]
  #:property prop:rename-transformer (struct-field-index rename-transformer)
  #:property prop:match-expander (struct-field-index match-expander))
