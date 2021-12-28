#lang sham/cryptol

#:compile-with sham

(test ap0 (== (<> [0] [1]) [(: 0 [64]) 1]))
