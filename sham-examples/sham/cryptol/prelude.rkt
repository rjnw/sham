#lang racket
;; https://github.com/GaloisInc/cryptol/blob/master/lib/Cryptol.cry
(provide (all-defined-out))
(define primitive-typeofs-stx
  #`(def
      [true false : bit]
      [zero : {a} (zero a) => a]
      [&& || ^ : {a} (logic a) => a -> a -> a]
      [~ : {a} (logic a) => a -> a]


      ;; arithmetic
      [+ - * : {a} (ring a) => a -> a -> a]
      [negate : {a} (ring a) => a -> a]
      [from-integer : {a} (ring a) (integral a) => integer -> a]
      [^^ : {a e} (ring a) (integral e) => a -> e -> a]
      [abs : {a} (cmp a) (ring a) => a -> a]
      [/ % : {a} (integral a) => a -> a -> a]
      [to-integer : {a} (integral a) => a -> integer]

      [== != : {a} (Eq a) => a -> a -> bit]
      [< > <= >= : {a} (cmp a) => a -> a -> bit]

      ;; sequences
      [take : {front back a} (fin front) => [(+ front back) a] -> [front a]]
      [drop : {front back a} (fin front) => [(+ front back) a] -> [back a]]
      ;; changed # from cryptol to <> as racket doesn't like "#"
      [<> : {front back a} (fin front) => [front a] -> [back a] -> [(+ front back) a] ]
      [join : {parts each a} (fin each) => [parts [each a]] -> [(* parts each) a]]
      [split : {parts each a} (fin each) => [(* parts each) a] -> [parts [each a]]]
      [transpose : {rows cols a} => [rows [cols a]] -> [cols [rows a]]]
      [reverse : {n a} (fin n) => [n a] -> [n a]]
      [@ : {n a ix} (integral ix) => [n a] -> ix -> a]
      [! : {n a ix} (fin n) (integral ix) => [n a] -> ix -> a]
      [update : {n a ix} (fin ix) => [n a] -> [ix] -> a -> [n a]]
      [updateEnd : {n a ix} (fin n) (integral ix) => [n a] -> ix -> a -> [n a]]


      ;; shifting and rotating
      [<<< : {n ix a} (fin n) (integral ix) => [n a] -> ix -> [n a]]
      [>>> : {n ix a} (fin n) (integral ix) => [n a] -> ix -> [n a]]
      [>> : {n ix a} (integral ix) (zero a) => [n a] -> ix -> [n a]]
      [<< : {n ix a} (integral ix) (zero a) => [n a] -> ix -> [n a]]
      [>>$ : {n ix} (fin n) (>= n 1) (integral ix) => [n] -> ix -> [n]]
      ))

(define prelude-defs-stx
  #`(def
      ;; bitwise and logical operators
      [bit-impl bit-and bit-or : bit -> bit -> bit]
      [bit-impl a b = (cond [a b] [else true])]
      [bit-and a b = (cond [x y] [else false])]
      [bit-or a b = (cond [x true] [else y])]
      ;; comparisons

      ;; [<$ >$ <=$ >=$ : {a} (signed-cmp a) => a -> a -> bit]
      [min max : {a} (cmp a) => a -> a -> a]
      [min x y = (cond [(< x y) x] [else y])]
      [max x y = (cond [(> x y) x] [else y])]

      ;; [=== !== : {a b} (eq b) => (a -> b) -> (a -> b) -> a -> bit]



      ;; sequences
      [groupBy : {each parts a} (fin each) => [(* parts each) a] -> [parts [each a]]]
      [groupBy s = (split {parts each a} s)]

      [head : {n a} => [(+ 1 n) a] -> a]
      [head xs = (@ xs 0)]
      [tail : {n a} => [(+ 1 n) a] -> [n a]]
      [tail xs = (drop {1} xs)]
      [last : {n a} (fin n) => [(+ 1 n) a] -> a]
      [last xs = (! xs 0)]

      ;; indexing and updates
      [@@ : {n k ix a} (integral ix) => [n a] -> [k ix] -> [k a]]
      [@@ xs is = [(@ xs i) | i <- is]]
      [!! : {n k ix a} (fin n) (integral ix) => [n a] -> [k ix] -> [k a]]
      [!! xs is = [(! xs i) | i <- is]]
      ;; [updates : {n k a ix} (integral ix) (fin k) => [n a] -> [k ix] -> [k a] -> [n a]]
      ;; [updatesEnd : {n k a ix} (fin n) (integral ix) (fin k) => [n a] -> [k ix] -> [k a] -> [n a]]


      ;; fp

      ;; [iterate : {n} -> (a -> a) -> a -> [inf a]]
      ;; [repeat : {n a} => a -> [n a]]
      ;; [map : {n a b} => (a -> b) -> [n a] -> [n b]]
      ;; [zip : {n a b} => [n a] -> [n b] -> [n b]]
      ;; [zipwith : {n a b} => (a -> b -> c) -> [n a] -> [n b] -> [n c]]
      ;; [foldl : {n a b} (fin n) => (a -> b -> a) -> a -> [n b] -> a]

      ;; exceptions

      ;; [undefined : {a} a]
      ;; [error : {a n} (fin n) => (string n) -> a]
      ))
