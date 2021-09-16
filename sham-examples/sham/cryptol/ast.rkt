#lang racket

(require sham/sam/ast)
(provide (all-defined-out))

(define-ast cry-ast
  (top
   [mod (id_name
         ;; (parameter_ps ...)
         decl_body ...)])

  #;(parameter
     [type (id kind (~optional value))]
     [value (id type (~optional value))]
     [constraint (c)]
     #:alias p)

  (def
    [val (t bs:def.bind ...)]
    [type (id_name t_val)]
    [typeof (id_name t_val)]
    ;; [import (id_name id_qualifier (id_only ...) (id_hide ...) (parameter_ps ...))]
    ;; [mod-inst (id_name id_of (p_ps ...) (d_ds ...))]
    ;; [private (d_ds ...)]
    [bind ((pat_ps ...) e_body)]
    #:alias d)

  (pat
   [var (id_name)]
   [tuple (pat_ps ...)]
   [sequence (pat_ps ...)])

  (expr
   [app (e_o
         (e_t ...)
         e_rands ...)]
   [cond ((e_chk e_then) ... e_else)]
   [var (id_name)]
   [tvar (id_name)]
   [annot (t e)]
   [where (e_body d_defs ...)]
   [error (msg)]
   [lit (integer_v)]
   [char (char_c)]
   #:alias e)

  #;(record expr
            [tuple (e ...)]
            [literal ((id_field e_val) ...)]
            [project (e (~ (~or id integer) field))]
            [update (e (id_fields e_vals) ...)])

  (bit expr [true] [false])

  (numeric expr
           [zero]
           ;; [inf]
           ;; [polynomial ((integer_e integer_i) ...)] ;; polynomial expression e*x^i ...
           [int (~ integer val)])

  (sequence expr
            [basic (e_val ...)]
            [enum (e_from e_step e_to)]
            ;; sequence comprehension [expr | pat <- expr, ... | ...]
            [comp (e_body ((pat_ps e_vs) ...) ...)]
            )

  (type
   [bit]                                ;; true & false
   [integer]                            ;; unbounded integers
   [sequence (dim t)]                   ;; collection of `dim` homogeneous elements
   [tuple (t ...)]
   [var (id_name)]
   [poly ((id_vars ...) t)]
   [constraint ((cs ...) t)]
   ;; [record ((id t) ...)]
   [func (t_from t_to)]
   #:alias t)

  (dim                                  ;; dimension for sequences: integer, infinity or a poly variable
   [int integer_v]
   ;; [inf]
   [app (id_rator dim_rands ...)]
   [var (id_name)]
   ))


;; (module+ test
;;   (define cry-foldl
;;     (cry:decl:def foldl (cry:type:poly [n a b] [(fin n)]
;;                                        (a -> ((a -> (b -> a)) -> ((sequence n b) -> a))))
;;                   (cry:decl:bind foldl [seed f xs]
;;                                  (cry:expr:where (cry:expr:app (cry:expr:app ! () res) cry:expr:zero)
;;                                                  (cry:decl:bind res []
;;                                                                 (cry:expr:app
;;                                                                  (cry:expr:app #() (cry:expr:sequence:basic seed))
;;                                                                  ()
;;                                                                  (cry:expr:sequence:comp
;;                                                                   (cry:expr:app (cry:expr:app f () a) () x)
;;                                                                   [a res]
;;                                                                   [x xs])))))))
;;   (define cry-fold-pretty
;;     ($cry (def foldl ({n a b} [(fin n)] (a -> (a -> (b -> a)) -> (seq n b) -> a))
;;             (foldl (seed f xs)
;;                    (where (! res zero)
;;                           [res (# seed [(f a x) | a <- res | x <- xs])]))))))
