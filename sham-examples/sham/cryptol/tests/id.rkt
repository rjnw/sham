#lang sham/cryptol

#:compile-with sham

(def [id : {a} a -> a]
  [id a = a])
(test id0 (== (id true) true))
