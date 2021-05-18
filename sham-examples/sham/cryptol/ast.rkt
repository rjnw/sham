#lang racket

(require sham/sam/ast)

(define-ast cry
  (top
   [mod (id (ps:parameter ...) (is:import ...) decl_body ...)]
   [import (name:id qualifier:id (only:id ...) (hide:id ...) (ps:parameter ...))])

  (parameter
   [type (id kind (~optional value))]
   [value (id type (~optional value))]
   [constraint (c)]
   #:alias p)

  (decl
   [mod-inst (id_name id_of (p_ps ...) (d_ds ...))]
   [type (id_name (id_vars ...) t_body)]
   [newtype (id_name (id_vars ...) t_body)]
   [def (id_name t body:decl.bind ...)]
   [private (d_ds ...)]
   [bind (id_name (vars_id ...) e_body)]
   #:alias d)

  (expr
   [annot (e t)]
   [app (e_o (e_t ...) e_a)]
   [if ((e_cond e_then) ... e_else)]
   [var id]
   [type-var id]
   [qualified-var (id_from id_var)]
   [where (e_body bs:decl.bind ...)]
   [zero]
   #:alias e)

  (record expr
   [tuple (e ...)]
   [literal ((id_field e_val) ...)]
   [project (e (~ (~or id integer) field))]
   [update (e (id_fields e_vals) ...)])

  (bit expr [true] [false])

  (numeric expr
           [polynomial ((i:int e:int) ...)] ;; polynomial expression e*x^i ...
           [int int_val])

  (sequence expr
            [basic (e_val ...)]
            [enum ((~ (~optional e) step) e_from (~ (~optional e) to))] ;; missing to = infinity
            ;; sequence comprehension [e | var <- val, ... | ...]
            [comp (e_body ((id_var e_val) ...) ...)])

  (type
   [bit]                                ;; true & false
   [integer]                            ;; unbounded integers
   [sequence (dim t)]                   ;; collection of `dim` homogeneous elements
   [record ((id t) ...)]
   [tuple (t ...)]
   [named id]
   ;; {vars, ...} (cop cvars ...) ... => body
   [poly ([id_vars ...] [(id_cop id_cvars ...) ...] t_body)]
   [func (t_from '-> t_to)]
   #:alias t)

  (dim                                  ;; dimension for sequences: integer, infinity or a poly variable
   [int (? positive-integer?)]
   [inf]
   [var id]))

(module+ test
  (define cry-foldl
    (cry:decl:def foldl (cry:type:poly [n a b] [(fin n)]
                                       (a -> ((a -> (b -> a)) -> ((sequence n b) -> a))))
                  (cry:decl:bind foldl [seed f xs]
                                 (cry:expr:where (cry:expr:app (cry:expr:app ! () res) cry:expr:zero)
                                                 (cry:decl:bind res []
                                                                (cry:expr:app
                                                                 (cry:expr:app # () (cry:expr:sequence:basic seed))
                                                                 ()
                                                                 (cry:expr:sequence:comp
                                                                  (cry:expr:app (cry:expr:app f () a) () x)
                                                                  [a res]
                                                                  [x xs])))))))
  (define cry-fold-pretty
    ($cry (def foldl ({n a b} [(fin n)] (a -> (a -> (b -> a)) -> (seq n b) -> a))
            (foldl (seed f xs)
                   (where (! res zero)
                          [res (# seed [(f a x) | a <- res | x <- xs])]))))))