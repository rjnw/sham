#lang sham/cryptol
;;  https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Keyless/Hash/SHA1.cry
#:compile-with sham

(def
  [sha1 : {n} (<= (width (* 8 n)) 64) => [n [8]] -> [160]]
  [sha1 msg = (sha1^ pmsg)
        [pmsg : [(/^ (+ (* n 8) 65) 512) [512]]]
        [pmsg = (pad (join msg))]])

(def
  [sha1^ : {chunks} (fin chunks) => [chunks [512]] -> [160]]
  [sha1^ pmsg = (join (! Hs 0))
         [Hs : [(+ chunks 1) [5 [32]]]]
         [Hs = (<>
                [[#x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0]]
                [(block H (split M))
                 | H <- Hs
                 | M <- pmsg])]])

(def
  [pad : {msgLen} (fin msgLen) (>= 64 (width msgLen))
       => [msgLen] -> [(/^ (+ msgLen 65) 512) [512]]]
  [pad msg = (split (<> msg [True] (: zero [padLen]) (: 'msgLen [64])))
       [type padLen = (%^ (+ 'msgLen 65) 512)]])

(def
  [f : #([8] [32] [32] [32]) -> [32]]
  [f #(t x y z) =
     (cond [(bit-and (<= 0 t) (<= t 19)) (^ (&& x y) (&& (~ x) z))]
           [(bit-and (<= 20 t) (<= t 39)) (^ x (^ y z))]
           [(bit-and (<= 40 t) (<= t 59)) (^ (&& x y) (^ (&& x z) (&& y z)))]
           [(bit-and (<= 60 t) (<= t 79)) (^ x (^ y z))]
           [else (error "f: t out of range")])])

(def
  [Ks : [80 [32]]]
  [Ks = [<>
         [#x5a827999 | t <- [0 .. 19]]
         [#x6ed9eba1 | t <- [20 .. 39]]
         [#x8f1bbcdc | t <- [40 .. 59]]
         [#xca62c1d6 | t <- [60 .. 79]]]])

(def
  [block : #([5 [32]] [16 [32]]) -> [5 [32]]]
  [block #([H0 H1 H2 H3 H4] M) = [(+ H0 (@ As 80))
                                  (+ H1 (@ Bs 80))
                                  (+ H2 (@ Cs 80))
                                  (+ H3 (@ Ds 80))
                                  (+ H4 (@ Es 80))]
         [Ws : [80 [32]]]
         [Ws = (<> M
                   [(<<< (^ W3 W8 W14 W16) 1)
                    | W16 <- (drop {(- 16 16)} Ws)
                    | W14 <- (drop {(- 16 14)} Ws)
                    | W8 <- (drop {(- 16 8)} Ws)
                    | W3 <- (drop {(- 16 3)} Ws)
                    | t <- [16..79]])]
         [As = (<> [H0] TEMP)]
         [Bs = (<> [H1] As)]
         [Cs = (<> [H2] [(<<< B 30) | B <- Bs])]
         [Ds = (<> [H3] Cs)]
         [Es = (<> [H4] Ds)]
         [TEMP : [80 [32]]]
         [TEMP = [(+ (<<< A 5) (f t B C D) E W K)
                  | A <- As
                  | B <- Bs
                  | C <- Cs
                  | D <- Ds
                  | E <- Es
                  | W <- Ws
                  | K <- Ks
                  | t <- [0 .. 79]]]])

(test t0 (== (sha1 "") #xda39a3ee5e6b4b0d3255bfef95601890afd80709))
(test t1 (== (sha1 "abc") #xA9993E364706816ABA3E25717850C26C9CD0D89D))
(test t2 (== (sha1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") #x84983E441C3BD26EBAAE4AA1F95129E5E54670F1))
(test t3 (== (sha1 [#\a | i <- [1 .. 1000000]]) #x34AA973CD4C4DAA4F61EEB2BDBAD27316534016F))
