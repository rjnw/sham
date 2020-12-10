#lang racket

(provide (all-defined-out))

(struct ast (groups info) #:prefab)
(struct ast:group (id parent nodes info) #:prefab)
(struct ast:node (id pattern info) #:prefab)

(struct ast:pat:single   (type id) #:prefab)
(struct ast:pat:datum    (syn) #:prefab)
(struct ast:pat:multiple (specs) #:prefab)
(struct ast:pat:repeat   (spec) #:prefab)
(struct ast:pat:checker  (check id) #:prefab)
