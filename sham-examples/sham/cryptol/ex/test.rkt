#lang sham/cryptol

#:compile-with ir

;; (def [f : ([a] -> [b])])
(def [f : ([a] -> [64])])
;; (def [f : [a] -> [b]])
;; (def [f : {a b} [a] -> [b]])

;; (def [f : [a] -> ([b] -> [c])])
;; (def [f : [a] -> [b] -> [c]])
;; (def [f : {a b c} [a] -> [b] -> [c]])
;; (def [f : ({a b c} [a] -> [b] -> [c])])


;; (def [f : (a -> b) -> c])
;; (def [map : {n a b} (a -> b) -> [n a] -> [n b]])

;; (def [cs : {a b} (and (check1 a) (check2 b)) => a -> b])

;; (def [cs : {a b} (check1 a) (check2 b) => a -> b])
;; (def
;;   [pad : {msgLen} (fin msgLen) (>= 64 (width msgLen))
;;        => [msgLen] -> [(/^ (+ msgLen 65) 512) [512]]]
;;   [pad msg = (split (<> msg [True] (: zero [padLen]) (: 'msgLen [64])))
;;        [type padLen = (%^ (+ msgLen 65) 512)]])
(def
  [sha1^ : {chunks} (fin chunks) => [chunks [512]] -> [160]]
  [sha1^ pmsg = (join (! Hs 0))
         [Hs : [(+ chunks 1) [5 [32]]]]
         [Hs = (<>
                [[#x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0]]
                [(block H (split M))
                 | H <- Hs
                 | M <- pmsg])]])
