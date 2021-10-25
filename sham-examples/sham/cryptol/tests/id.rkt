#lang sham/cryptol

#:compile-with sham

(def
  [id : {a} a -> a]
  [id a = a])
(test id-pass (== (id true) true))
(test id-fail (== (id true) false))
