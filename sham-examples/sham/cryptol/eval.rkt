#lang racket

(define-compiler (cry-compiler (st '())) (cry -> post)
  (ct (top -> top)
      [(mod id (ps ...) (is ...) decl ...)
       (post:module id (map c->p decl))])

  (ce (expr -> expr)
      [(annot e t) (ce e)]
      [(app op (t ...) a) (post:app (ce op) (map (compose lift-type ct) t) (ce a))]
      [(if (c t) ... e)
       (post:cond [(ce c) (ce t)] ... [else (ce e)])]
      [(var v) (post:var v)]
      [(type-var t) (lift-type t)]
      [(qualified-var fr v) (post:var v)]
      [(where e (bind n (v ...) b) ...)
       (post:let ([n (post:lambda (v ...) (ce b))] ...)
                 (ce b))]
      [(zero) (post:int 0)])
  (ct (type -> *)
      []))
