#lang sham/cryptol

(def [f : ([a] -> [b])])
(def [f : [a] -> [b]])
(def [f : {a b} [a] -> [b]])

(def [f : [a] -> ([b] -> [c])])
(def [f : [a] -> [b] -> [c]])
(def [f : {a b c} [a] -> [b] -> [c]])
(def [f : ({a b c} [a] -> [b] -> [c])])


(def [f : (a -> b) -> c])
(def [map : {n a b} (a -> b) -> [n a] -> [n b]])

(def [cs : {a b} (and (check1 a) (check2 b)) => a -> b])

(def [cs : {a b} (check1 a) (check2 b) => a -> b])
