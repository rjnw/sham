#lang sham/cryptol

#:compile-with sham

(test t0 (== [42 | i <- [0 .. 1]]
             [(: 42 [64]) 42]))

(test tt1 (== [i | i <- [0 .. 1]]
              [(: 0 [64]) 1]))

(test tt2 (== [#(i j)
               | i <- [0 .. 1]
               | j <- [2 .. 3]]
              [#((: 0 [64]) (: 2 [64])) #(1 3)]))
