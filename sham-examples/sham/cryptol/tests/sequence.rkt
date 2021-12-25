#lang sham/cryptol

#:compile-with sham

(test str2 (== "ab" [#x61 #x62]))
(test str11 (== ["a"] ["a"]))
(test str3 (== "abc" [#x61 #x62 #x63]))
(test ss1 (== [["a"] ["b"] ["c"]] [["a"] ["b"] ["c"]]))

(def
  [const1 : {a} a -> [1 a]]
  [const1 v = [v]])

(test c1-bit (== (const1 (: 42 [64])) [42]))
(test c1-bit-fail (== (const1 (: 42 [64])) [41]))

(test c1-bbit (== (const1 (: [42] [1 [64]])) [[42]]))
(test c1-bit (== (const1 (: 42 [64])) [42]))
(test c1-bbit (== (const1 (: [42] [1 [64]])) [[42]]))

(def
  [proj1 : {a} [1 a] -> a]
  [proj1 [v] = v])

(test p1-bit (== (proj1 (: [42] [1 [64]])) 42))
(test p1-bbit (== (proj1 [(: [42] [1 [64]])]) (: [42] [1 [64]])))
