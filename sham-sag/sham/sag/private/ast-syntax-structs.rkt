#lang racket

(provide (all-defined-out))

(struct ast (groups info) #:prefab)
(struct ast:group (name parent nodes info) #:prefab)
(struct ast:node (variable pattern info) #:prefab)
(struct ast:term-node (variable proc) #:prefab)

(struct ast:pat:single   (type id) #:prefab)
(struct ast:pat:datum    (syn) #:prefab)
(struct ast:pat:multiple (specs) #:prefab)
(struct ast:pat:repeat   (spec) #:prefab)
(struct ast:pat:checker (check id) #:prefab)

(struct ast:node-contract [spec] #:prefab)
