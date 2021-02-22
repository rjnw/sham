#lang racket

(struct ast (groups info) #:prefab)
(struct ast:group (id parent nodes info) #:prefab)
(struct ast:node (id pattern info) #:prefab)

(module pattern racket
  (provide (all-defined-out))
  (struct ast:pat () #:prefab)
  (struct ast:pat:single ast:pat (id) #:prefab)
  (struct ast:pat:datum ast:pat (syn) #:prefab)
  (struct ast:pat:multiple ast:pat (specs) #:prefab)
  (struct ast:pat:repeat ast:pat (spec k-or-more) #:prefab)
  (struct ast:pat:checker ast:pat (check id) #:prefab))

(require (submod "." pattern))
(provide (all-defined-out)
         (all-from-out (submod "." pattern)))
