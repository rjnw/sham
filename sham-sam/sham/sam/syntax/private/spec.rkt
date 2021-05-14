#lang racket

(struct ast (groups info) #:prefab)
(struct ast:group (id parent nodes info) #:prefab)
(struct ast:node (id pattern info) #:prefab)

(module pattern racket
  (provide (all-defined-out))
  (struct ast:pat () #:prefab)
  (struct ast:pat:datum ast:pat (syn) #:prefab)
  (struct ast:pat:single ast:pat (maybe-check maybe-id) #:prefab)
  (struct ast:pat:multiple ast:pat (specs) #:prefab)
  (struct ast:pat:repeat ast:pat (spec count) #:prefab)) ;; repeat count is (cons min max) with #f for no limit

(module type racket
  (provide (all-defined-out))
  (struct ast:type (depth) #:prefab)
  (struct ast:type:internal ast:type (of) #:prefab)
  (struct ast:type:check ast:type (chk) #:prefab)
  (struct ast:type:identifier ast:type () #:prefab)
  (struct ast:type:intrinsic ast:type (kind) #:prefab)
  (struct ast:type:unknown ast:type () #:prefab))

(module reader racket
  (provide (all-defined-out))
  (struct reader (id info)))

(require (submod "." pattern))
(provide (all-defined-out)
         (all-from-out (submod "." pattern)))
