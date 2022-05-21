#lang sham/cryptol

#:compile-with sham

(def [foldp : {n} (fin n) => [64] -> [n [64]] -> [64]]
  [foldp seed xs = (! res 0)
         [res : [(+ n 1) [64]]]
         [res = (<> [seed]
                    (: [(+ a x) | a <- res | x <- xs] [n [64]]))]])

;; (test f0 (== (foldp (: 0 [64]) [0 .. 10]) #x37))
;; (test f1 (== (foldp (: 0 [64]) [0 .. 100]) #x13ba))
;; (test f2 (== (foldp (: 0 [64]) [0 .. 1000]) #x7a314))
;; (test f3 (== (foldp (: 0 [64]) [0 .. 10000]) #x2fb0408))
(test f4 (== (foldp (: 0 [64]) [0 .. 100000]) #x12a06b550))

;; (test f1 (== (foldp (: 0 [64]) [0 .. 10000]) 10))

;; (def [foldl : {n a b} (fin n) => a -> (a -> b -> a) -> [n b] -> a]
;;   [foldl seed f xs = (! res 0)
;;          [res : [(+ n 1) a]]
;;          [res = (<> [seed] [(f a x) | a <- res | x <- xs])]])

;; (def [plus : [64] -> [64] -> [64]] [plus x y = (+ x y)])

;; (test f0 (== (foldl (: 0 [64]) plus [0 1 2 3 4]) 10))
