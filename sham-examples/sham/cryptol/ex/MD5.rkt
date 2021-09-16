#lang sham/cryptol
;;  https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Keyless/Hash/MD5.cry

(def [ntohl : [32] -> [32]]
  [ntohl w = (join (reverse (groupBy {8} w)))])

(def [htonl : [32] -> [32]]
  [htonl w = (join (reverse (groupBy {8} w)))])

(def [htonll : [64] -> [64]]
  [htonll w = (join (reverse (groupBy {8} w)))])

(def [map : {n a b} (a -> b) -> [n a] -> [n b]]
  [map f xs = [(f x) | x <- xs]])

(def [foldl : {n a b} (fin n) => a -> (a -> b -> a) -> [n b] -> a]
  [foldl seed f xs = (! res 0)
         [res = (<> [seed] [(f a x) | a <- res | x <- xs])]])

(def [md5_ref : [16 [8]] -> [16 [8]]]
  [md5_ref msg = (map reverse (groupBy {8} (md5 (join (map reverse msg)))))])

(type [MD5State = #([32] [32] [32] [32])])

(def [md5 : {a} (64 >= width a) => [a] -> [128]]
  [md5 msg = (md5output finalState)
       [type p = (%^ (+ a 65) 512)]
       [type b = (/^ (+ a 65) 512)]
       [finalState : MD5State]
       [finalState = (foldl initialMD5State processBlock blocks)]
       [blocks : [b [512]]]
       [blocks = (groupBy {512} (pad {a p} msg))]

       [add : MD5State -> MD5State -> MD5State]
       [add #(a b c d) #(e f g h) = #((+ a e) (+ b f) (+ c g) (+ d h))]

       [processBlock : MD5State -> [512] -> MD5State]
       [processBlock st blk = (add st (computeRounds (decodeBlock blk) st))]])

(def [initialMD5State : MD5State]
  [initialMD5State = #(A B C D)
                   [f x = (ntohl (join x))]
                   [A = (f [ 0x01 0x23 0x45 0x67 ])]
                   [B = (f [ 0x89 0xAB 0xCD 0xEF ])]
                   [C = (f [ 0xFE 0xDC 0xBA 0x98 ])]
                   [D = (f [ 0x76 0x54 0x32 0x10 ])]])

(def [decodeBlock : [512] -> [16 [32]]]
  [decodeBlock s = (map ntohl (groupBy {32} s))])

(def [md5output : MD5State -> [128]]
  [md5output #(a b c d) = (<> (htonl a) (htonl b) (htonl c) (htonl d))])

(def [pad : {a p} (fin p) (>= 64 width a) => [a] -> [(+ 65 a p)]]
  [pad msg = (<> msg [True] zero (htonll sz))
       [sz : [64]]
       [sz = (length msg)]])

(def [computeRounds : #([16 [32]] MD5State) -> [65 MD5State]]
  [computeRounds msg st = (@ (<> msg st) 64)])

(def [rounds : #([16 [32]] MD5State) -> [65 MD5State]]
  [rounds #(msg #(a0 b0 c0 d0)) = [#(a b c d) | a <- as | b <- bs | c <- cs | d <- ds]
          [bs = [<> b0
                    [(box #(i a b c d m t s))
                     | i <- [0 .. 63]
                     | a <- as
                     | b <- bs
                     | c <- cs
                     | d <- ds

                     | m <- join [(@@ m p)
                                  | m <- [msg msg msg msg]
                                  | p <- permutes]
                     | t <- sineTbl
                     | s <- s_constants
                     ]]]
          [cs = [<> [c0] bs]]
          [ds = [<> [d0] cs]]
          [as = [<> [a0] ds]]])


(def [s_constants : [64 [6]]]
  [s_constants = [<> (repeat4 [7 12 17 22])
                     (repeat4 [5 9 14 20])
                     (repeat4 [4 11 16 23])
                     (repeat4 [6 10 15 21])]]
  [repeat4 abcd = [<> abcd abcd abcd]])

(def [permutes : [4 [16 [4]]]]
  [permutes = [[0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15]
               [1  6 11  0  5 10 15  4  9 14  3  8 13  2  7 12]
               [5  8 11 14  1  4  7 10 13  0  3  6  9 12 15  2]
               [0  7 14  5 12  3 10  1  8 15  6 13  4 11  2  9]]])

(def
  [box : #([6] [32] [32] [32] [32] [32] [32] [6]) -> [32]]
  [box #(i a b c d m t s) = (+ b (+ a (boxfunc #(i b c d)) m (<<< t s)))])

(def [boxfunc : #([6] [32] [32] [32]) -> [32]]
  [boxfunc #(i b c d) =
           (cond [(< i 16) (F #(b c d))]
                 [(< i 32) (G #(b c d))]
                 [(< i 48) (H #(b c d))]
                 [else (I #(b c d))])])

(def  [F : #([32] [32] [32]) -> [32]]
  [F #(x y z) =
     (|| (&& x y)
         (&& (~ x) z))])

(def [G : #([32] [32] [32]) -> [32]]
  [G #(x y z) = (|| (&& x z) (&& y (~ z)))])

(def [H : #([32] [32] [32]) -> [32]]
  [H #(x y z) = (^ x  y  z)])

(def [I : #([32] [32] [32]) -> [32]]
  [I #(x y z) = (^ y (|| x (~ z)))])


(def
  [sineTbl : [64 [32]]]
  [sineTbl = [
              0xD76AA478 0xE8C7B756 0x242070DB 0xC1BDCEEE 0xF57C0FAF
              0x4787C62A 0xA8304613 0xFD469501 0x698098D8 0x8B44F7AF
              0xFFFF5BB1 0x895CD7BE 0x6B901122 0xFD987193 0xA679438E
              0x49B40821 0xF61E2562 0xC040B340 0x265E5A51 0xE9B6C7AA
              0xD62F105D 0x02441453 0xD8A1E681 0xE7D3FBC8 0x21E1CDE6
              0xC33707D6 0xF4D50D87 0x455A14ED 0xA9E3E905 0xFCEFA3F8
              0x676F02D9 0x8D2A4C8A 0xFFFA3942 0x8771F681 0x6D9D6122
              0xFDE5380C 0xA4BEEA44 0x4BDECFA9 0xF6BB4B60 0xBEBFBC70
              0x289B7EC6 0xEAA127FA 0xD4EF3085 0x04881D05 0xD9D4D039
              0xE6DB99E5 0x1FA27CF8 0xC4AC5665 0xF4292244 0x432AFF97
              0xAB9423A7 0xFC93A039 0x655B59C3 0x8F0CCC92 0xFFEFF47D
              0x85845DD1 0x6FA87E4F 0xFE2CE6E0 0xA3014314 0x4E0811A1
              0xF7537E82 0xBD3AF235 0x2AD7D2BB 0xEB86D391]])

(def [run s = (md5 (join s))])

(test r0 (== (run "") 0xd41d8cd98f00b204e9800998ecf8427e))
(test r1 (== (run "a") 0x0cc175b9c0f1b6a831c399e269772661))
(test r2 (== (run "abc") 0x900150983cd24fb0d6963f7d28e17f72))
(test r3 (== (run "message digest") 0xf96b697d7cb7938d525a2f31aaf161d0))
(test r4 (== (run "abcdefghijklmnopqrstuvwxyz") 0xc3fcd3d76192e4007dfb496cca67e13b))
(test r5 (== (run "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") 0xd174ab98d277d9f5a5611c2c9f419d9f))
(test r6 (== (run "12345678901234567890123456789012345678901234567890123456789012345678901234567890") 0x57edf4a22be3c955ac49da2e2107b67a))
(test r7 (== (run "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") 0x8215ef0796a20bcaaae116d3876c664a))
(test r8 (== (run (join (repeat {1000000} "a"))) 0x7707d6ae4e027c70eea2a935c2296f21))
