#lang racket

(struct ast (groups info) #:prefab)
(struct ast:group (id parent nodes info) #:prefab)
(struct ast:node (id pattern info) #:prefab)

(module pattern racket
  (provide (all-defined-out))
  (struct ast:pat:single   (type id) #:prefab)
  (struct ast:pat:datum    (syn) #:prefab)
  (struct ast:pat:multiple (specs) #:prefab)
  (struct ast:pat:repeat   (spec) #:prefab)
  (struct ast:pat:checker  (check id) #:prefab))

(require (submod "." pattern))
(provide (all-defined-out)
         (all-from-out (submod "." pattern)))
