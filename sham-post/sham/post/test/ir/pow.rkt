#lang racket

(require sham/post)

(decl-syntax
 (mod test-id-mod
      (sig (idf (fun int int)))
      (val idf (fun int int)
           (lam (a) a))))
