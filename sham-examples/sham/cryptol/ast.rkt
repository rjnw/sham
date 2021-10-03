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
    [gen (d_ds)]
    [val (id_name e_val)]
    [type (id_name t_val)]
    [typeof (id_name t_val)]
    [test (id_name e_v1 e_v2)]
    ;; [import (id_name id_qualifier (id_only ...) (id_hide ...) (parameter_ps ...))]
    ;; [mod-inst (id_name id_of (p_ps ...) (d_ds ...))]
    ;; [private (d_ds ...)]
    #:alias d)

  (pat
   [var (id_name)]
   [tuple (pat_ps ...)]
   [sequence (pat_ps ...)])

  (expr
   [bind ((pat_ps ...) e_body)]
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
   [tuple (e_vals ...)]
   [zero ()]
   #:alias e)

  (bit expr [true] [false])

  (sequence expr
            [basic (e_val ...)]
            [enum (e_from e_step e_to)]
            [str string_s]
            ;; sequence comprehension [expr | pat <- expr, ... | ...]
            [comp (e_body ((pat_ps e_vs) ...) ...)])

  #;(record expr
            [tuple (e ...)]
            [literal ((id_field e_val) ...)]
            [project (e (~ (~or id integer) field))]
            [update (e (id_fields e_vals) ...)])
  ;; (numeric expr
  ;;          [zero]
  ;;          ;; [inf]
  ;;          ;; [polynomial ((integer_e integer_i) ...)] ;; polynomial expression e*x^i ...
  ;;          [int (~ integer val)])

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

  ;; dimension for sequences: integer, infinity or a poly variable
  (dim
   [int integer_v]
   ;; [inf]
   [app (id_rator dim_rands ...)]
   [var (id_name)]))
