#lang sham/cryptol

#:compile-with sham

(def
  [id : {a} a -> a]
  [id a = a])
(test id-i64 (== (id (: 42 [64])) 42))
(test id-i32 (== (id (: 42 [32])) 42))
(test id-i8 (== (id (: 42 [8])) 42))
(test id-i512 (== (id (: 42 [512])) 42))
(test id-i160 (== (id (: #xda39a3ee5e6b4b0d3255bfef95601890afd80709 [160]))
                  #xda39a3ee5e6b4b0d3255bfef95601890afd80709))
