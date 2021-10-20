#lang sham/cryptol

#:compile-with sham

(def [id : {a} a -> a]
  [id a = a])
(test id0 (== (id true) true))

;; (def [v21 : #([32] [32] [32]) -> [32]]
;;   [v21 #(a b c) = a])
