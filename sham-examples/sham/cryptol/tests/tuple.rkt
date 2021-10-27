#lang sham/cryptol

#:compile-with sham

(def
  [const2 : {a} a -> a -> #(a a)]
  [const2 v1 v2 = #(v1 v2)])
(test c2-bits (== (const2 true false) #(true false)))

(def
  [proj1 : {a b} #(a b) -> a]
  [proj1 #(v1 v2) = v1])
(test p1-bits (== (proj1 #(true false)) true))

(def
  [proj2 : {a b} #(a b) -> b]
  [proj2 #(v1 v2) = v2])
(test p2-bits (== (proj2 #(true false)) false))

(def
  [proj11 : {a} #(#(a)) -> a]
  [proj11 #(#(v)) = v])
(test p11-bits (== (proj11 #(#(true))) true))

(def
  [const11 : {a} a -> #(#(a))]
  [const11 v = #(#(v))])
(test c11-bits (== (const11 true) #(#(true))))
