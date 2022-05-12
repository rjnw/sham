#lang racket

(require sham/sam/ast)
(provide (all-defined-out))

(define-ast cry-ir
  (def
    [val (id_name t_type e_val)]
    [type (id_name t_val)]
    [cfun (name t_type e_body)]

    [pfun (name (t_polyts ...) t_type)]
    [tst (id_name t_type e_v1 e_v2)])
  (expr
   [bnd (def_bs ... e_body)]
   [cnd ((chk thn) ... els)]
   [app (e_op (e_args ...))]
   [tup (e_vals ...)]
   [lit (v)]
   [seq-val (e_vals ...)]
   [seq-enm (f s t)]
   [seq-laz (id_idx e_val)]
   [tup-idx (e_val e_idx)]
   [seq-idx (e_val e_idx)]
   [seq-len (e_val)]
   [bnv (id_name)]
   [lid (id_name)]
   [fvar (fdef)]
   [pvar (id_name)]
   [frg (idx)]
   [err (str)]
   #:common t_type
   #:alias e)

  (type
   [bit]
   [int]
   [idx]
   [tup (t_ts ...)]
   [seq (dim t_elem)]
   [fun ((t_pargs ...) (t_vargs ...) t_to)]
   #:alias t)
  #:format (#f - #t - -))
