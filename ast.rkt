#lang racket

(require "private/ast.rkt"
         (submod "private/ast.rkt" utils))

(provide (all-from-out "private/ast.rkt"))
         ;(all-from-out (submod "private/ast.rkt" utils)))
;we could provide the utils submodule directly here,
;I don't know which one would be cleaner, for now utils as a submodule
;seems more elegant(reduces namespace cluttering)
(module* utils #f
  (require (submod "private/ast.rkt" utils))
  (provide (all-from-out (submod "private/ast.rkt" utils))))
