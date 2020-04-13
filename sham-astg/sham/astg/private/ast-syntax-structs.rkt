#lang racket

(provide (all-defined-out))

(struct ast:group (name parent nodes meta-info) #:prefab)
(struct ast:node (variable pattern meta-info) #:prefab)
(struct ast:term-node (variable proc))
(struct ast:pat:single   (spec) #:prefab)
(struct ast:pat:datum    (spec) #:prefab)
(struct ast:pat:multiple (spec) #:prefab)
(struct ast:pat:repeat   (spec) #:prefab)
(struct ast:pat:checker (check spec))
