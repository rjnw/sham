#lang sham/cryptol

#:compile-with ir
(def
  [f : #([8] [32] [32] [32]) -> [32]]
  [f #(t x y z) =
     (cond [(logic-and (<= 0 t) (<= t 19)) (^ (&& x y) (&& (~ x) z))]
           [(logic-and (<= 20 t) (<= t 39)) (^ x (^ y z))]
           [(logic-and (<= 40 t) (<= t 59)) (^ (&& x y) (^ (&& x z) (&& y z)))]
           [(logic-and (<= 60 t) (<= t 79)) (^ x (^ y z))]
           [else (error "f: t out of range")])])
