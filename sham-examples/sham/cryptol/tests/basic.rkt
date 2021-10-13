#lang sham/cryptol

#:compile-with test

(def [id : {a} a -> a]
  [id a = a])
(test id0 (== (id (: true bit)) true))

;; (def [v21 : #([32] [32] [32]) -> [32]]
;;   [v21 #(a b c) = a])
