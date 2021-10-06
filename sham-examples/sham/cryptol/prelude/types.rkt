#lang racket
;; https://github.com/GaloisInc/cryptol/blob/master/lib/Cryptol.cry

;; bitwise and logical operators
(defi
  [true false : bit]
  [&& || ^ : {a} (logic a) => a -> a -> a]
  [~ : {a} (logic a) => a -> a]
  [==> /\ \/ : bit -> bit -> bit])

;; comparisons
(defi
  [== != : {a} (Eq a) => a -> a -> bit]
  [< > <= >= : {a} (cmp a) => a -> a -> bit]
  [<$ >$ <=$ >=$ : {a} (signed-cmp a) => a -> a -> bit]
  [min max : {a} (cmp a) => a -> a -> a]
  [=== !== : {a b} (eq b) => (a -> b) -> (a -> b) -> a -> bit])

;; arithmetic
(defi
  [+ - * : {a} (ring a) => a -> a -> a]
  [negate : {a} (ring a) => a -> a]
  [from-integer : {a} (ring a) (integral a) => integer -> a]
  [^^ : {a e} (ring a) (integral e) => a -> e -> a]
  [abs : {a} (cmp a) (ring a) => a -> a]
  [/ % : {a} (integral a) => a -> a -> a]
  [to-integer : {a} (integral a) => a -> integer])

;; sequences
(defi
  [take : {front back a} (fin front) => [(+ front back) a] -> [front a]]
  [drop : {front back a} (fin front) => [(+ front back) a] -> [back a]]
  ;; changed # from cryptol to <> as racket doesn't like "#"
  [<> : {front back a} (fin front) => [front a] -> [back a] -> [(+ front back) a] ]
  [join : {parts each a} (fin each) => [parts [each a]] -> [(* parts each) a]]
  [split : {parts each a} (fin each) => [(* parts each) a] -> [parts [each a]]]
  [groupBy : {each parts a} (fin each) => [(* parts each) a] -> [parts [each a]]]
  [transpose : {rows cols a} => [rows [cols a]] -> [cols [rows a]]]
  [reverse : {n a} (fin n) => [n a] -> [n a]]
  [head : {n a} => [(+ 1 n) a] -> a]
  [tail : {n a} => [(+ 1 n) a] -> [n a]]
  [last : {n a} (fin n) => [(+ 1 n) a] -> a])

;; indexing and updates

(defi
  [@ : {n a ix} (integral ix) => [n a] -> ix -> a]
  [! : {n a ix} (fin n) (integral ix) => [n a] -> ix -> a]
  [@@ : {n k ix a} (integral ix) => [n a] -> [k ix] -> [k a]]
  [!! : {n k ix a} (fin n) (integral ix) => [n a] -> [k ix] -> [k a]]
  [update : {n a ix} (fin ix) => [n a] -> [ix] -> a -> [n a]]
  [updateEnd : {n a ix} (fin n) (integral ix) => [n a] -> ix -> a -> [n a]]
  [updates : {n k a ix} (integral ix) (fin k) => [n a] -> [k ix] -> [k a] -> [n a]]
  [updatesEnd : {n k a ix} (fin n) (integral ix) (fin k) => [n a] -> [k ix] -> [k a] -> [n a]])

;; shifting and rotating

(defi
  [<<< : {n ix a} (fin n) (integral ix) => [n a] -> ix -> [n a]]
  [>>> : {n ix a} (fin n) (integral ix) => [n a] -> ix -> [n a]]
  [>> : {n ix a} (integral ix) (zero a) => [n a] -> ix -> [n a]]
  [<< : {n ix a} (integral ix) (zero a) => [n a] -> ix -> [n a]]
  [>>$ : {n ix} (fin n) (>= n 1) (integral ix) => [n] -> ix -> [n]])

;; fp
(defi
  ;; [iterate : {n} -> (a -> a) -> a -> [inf a]]
  [repeat : {n a} => a -> [n a]]
  [map : {n a b} => (a -> b) -> [n a] -> [n b]]
  [zip : {n a b} => [n a] -> [n b] -> [n b]]
  [zipwith : {n a b} => (a -> b -> c) -> [n a] -> [n b] -> [n c]]
  [foldl : {n a b} (fin n) => (a -> b -> a) -> a -> [n b] -> a])

;; exceptions
(defi
  [undefined : {a} a]
  [error : {a n} (fin n) => (string n) -> a])
