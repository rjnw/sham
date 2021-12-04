#lang sham/cryptol

#:compile-with sham

(def
  [const1 : {a} a -> [1 a]]
  [const1 v = [v]])

;; (test c1-bit (== (const1 (: 42 [64])) [42]))
;; (test c1-bit-fail (== (const1 (: 42 [64])) [41]))

(test c1-bbit (== (const1 (: [42] [1 [64]])) [[42]]))
