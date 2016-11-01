#lang racket

(require ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))

;;utils
(define (pointer-to t)
  _pointer) ;;TODO

(define libjit (ffi-lib "libjit"))
(define-ffi-definer define-jit libjit)


;;jit-defs.h - complete
(define jit_sbyte _sbyte)
(define jit_ubyte _ubyte)

(define jit_short _short)
(define jit_ushort _ushort)

(define jit_int _int)
(define jit_uint _uint)

(define jit_nint _long)
(define jit_nuint _ulong)

(define jit_long _long)
(define jit_ulong _ulong)

(define jit_float32 _float)
(define jit_float64 _double)
(define jit_nfloat _longdouble)

(define jit_ptr _pointer)

(define char _byte)

;;jit-common.h - complete
(define jit_context_t _pointer)
(define jit_function_t _pointer)
(define jit_block_t _pointer)
(define jit_insn_t _pointer)
(define jit_value_t _pointer)
(define jit_type_t _pointer)
(define jit_stack_trace_t _pointer)
(define jit_label_t jit_nuint)


(define jit_meta_free_func (_fun _pointer -> _void))
(define jit_on_demand_func (_fun jit_function_t -> _int))
(define jit_on_demand_driver_func (_fun jit_function_t -> _pointer))
;;^^ typedef void *(*jit_on_demand_driver_func)(jit_function_t func);


;;jit-context.h --complete
(define-jit jit_context_create (_fun -> jit_context_t))
(define-jit jit_context_destroy (_fun jit_context_t -> _void))
(define-jit jit_context_build_start (_fun jit_context_t -> _void))
(define-jit jit_context_build_end (_fun jit_context_t -> _void))
(define-jit
  jit_context_set_on_demand_driver
  (_fun jit_context_t jit_on_demand_driver_func -> _void))
;; (define-jit
;;   jit_context_set_memory_manager
;;   (_fun jit_context_t jit_memory_manager_t -> _void))
;; TODO add jit_memory_manager_t type
(define-jit
  jit_context_set_meta
  (_fun jit_context_t _int (pointer-to _void) jit_meta_free_func -> _int))
(define-jit
  jit_context_set_meta_numeric
  (_fun jit_context_t _int jit_nuint -> _int))
(define-jit
  jit_context_get_meta
  (_fun jit_context_t _int -> (pointer-to _void)))
(define-jit
  jit_context_get_meta_numeric
  (_fun jit_context_t _int -> jit_nuint))
(define-jit jit_context_free_meta (_fun jit_context_t _int -> _void))

(define	JIT_OPTION_CACHE_LIMIT		10000)
(define	JIT_OPTION_CACHE_PAGE_SIZE	10001)
(define	JIT_OPTION_PRE_COMPILE		10002)
(define	JIT_OPTION_DONT_FOLD		10003)
(define JIT_OPTION_POSITION_INDEPENDENT	10004)
(define JIT_OPTION_CACHE_MAX_PAGE_FACTOR	10005)

;;jit-apply.h - complete
(define jit_closure_func (_fun jit_type_t _pointer _pointer _pointer -> _void))
(define jit_closure_va_list_t _pointer)

(define-jit jit_apply (_fun jit_type_t (pointer-to _void) (pointer-to _void)
                            _uint (pointer-to _void) ->
                            _void))
(define-jit jit_apply_raw
  (_fun jit_type_t (pointer-to _void) (pointer-to _void) (pointer-to _void) ->
        _void))
(define-jit jit_raw_supported (_fun jit_type_t -> _int))
(define-jit jit_closure_create
  (_fun jit_context_t jit_type_t jit_closure_func (pointer-to _void) ->
        (pointer-to _void)))
(define-jit jit_closure_va_get_nint (_fun jit_closure_va_list_t -> jit_nint))
(define-jit jit_closure_va_get_nuint
  (_fun jit_closure_va_list_t -> jit_nuint))
(define-jit jit_closure_va_get_long (_fun jit_closure_va_list_t -> jit_long))
(define-jit jit_closure_va_get_ulong
  (_fun jit_closure_va_list_t -> jit_ulong))
(define-jit jit_closure_va_get_float32
  (_fun jit_closure_va_list_t -> jit_float32))
(define-jit jit_closure_va_get_float64
  (_fun jit_closure_va_list_t -> jit_float64))
(define-jit jit_closure_va_get_nfloat
  (_fun jit_closure_va_list_t -> jit_nfloat))
(define-jit jit_closure_va_get_ptr
  (_fun jit_closure_va_list_t -> (pointer-to _void)))
(define-jit jit_closure_va_get_struct
  (_fun jit_closure_va_list_t (pointer-to _void) jit_type_t -> _void))

;;jit-block.h - complete
(define-jit jit_block_get_function (_fun jit_block_t -> jit_function_t))
(define-jit jit_block_get_context (_fun jit_block_t -> jit_context_t))
(define-jit jit_block_get_label (_fun jit_block_t -> jit_label_t))
(define-jit jit_block_get_next_label
  (_fun jit_block_t jit_label_t -> jit_label_t))
(define-jit jit_block_next (_fun jit_function_t jit_block_t -> jit_block_t))
(define-jit jit_block_previous
  (_fun jit_function_t jit_block_t -> jit_block_t))
(define-jit jit_block_from_label
  (_fun jit_function_t jit_label_t -> jit_block_t))
(define-jit jit_block_set_meta
  (_fun jit_block_t _int (pointer-to _void) jit_meta_free_func -> _int))
(define-jit jit_block_get_meta (_fun jit_block_t _int -> (pointer-to _void)))
(define-jit jit_block_free_meta (_fun jit_block_t _int -> _void))
(define-jit jit_block_is_reachable (_fun jit_block_t -> _int))
(define-jit jit_block_ends_in_dead (_fun jit_block_t -> _int))
(define-jit jit_block_current_is_dead (_fun jit_function_t -> _int))

;;jit-debugger.h

;;jit-elf.h

;;jit-except.h -complete
; Builtin exception type codes, and result values for intrinsic functions.
; TODO write these defines
;; define JIT_RESULT_OK			(1)
;; define JIT_RESULT_OVERFLOW		(0)
;; define JIT_RESULT_ARITHMETIC		(-1)
;; define JIT_RESULT_DIVISION_BY_ZERO	(-2)
;; define JIT_RESULT_COMPILE_ERROR	(-3)
;; define JIT_RESULT_OUT_OF_MEMORY	(-4)
;; define JIT_RESULT_NULL_REFERENCE	(-5)
;; define JIT_RESULT_NULL_FUNCTION	(-6)
;; define JIT_RESULT_CALLED_NESTED	(-7)
;; define JIT_RESULT_OUT_OF_BOUNDS	(-8)
;; define JIT_RESULT_UNDEFINED_LABEL	(-9)
;; define JIT_RESULT_MEMORY_FULL		(-10000)

(define jit_exception_func (_fun _int -> _pointer))

(define-jit jit_exception_get_last (_fun -> (pointer-to _void)))
(define-jit jit_exception_get_last_and_clear (_fun -> (pointer-to _void)))
(define-jit jit_exception_set_last (_fun (pointer-to _void) -> _void))
(define-jit jit_exception_clear_last (_fun -> _void))
(define-jit jit_exception_throw (_fun (pointer-to _void) -> _void))
(define-jit jit_exception_builtin (_fun _int -> _void))
(define-jit jit_exception_set_handler
  (_fun jit_exception_func -> jit_exception_func))
(define-jit jit_exception_get_handler (_fun -> jit_exception_func))
(define-jit jit_exception_get_stack_trace (_fun -> jit_stack_trace_t))
(define-jit jit_stack_trace_get_size (_fun jit_stack_trace_t -> _uint))
(define-jit jit_stack_trace_get_function
  (_fun jit_context_t jit_stack_trace_t _uint -> jit_function_t))
(define-jit jit_stack_trace_get_pc
  (_fun jit_stack_trace_t _uint -> (pointer-to _void)))
(define-jit jit_stack_trace_get_offset
  (_fun jit_context_t jit_stack_trace_t _uint -> _uint))
(define-jit jit_stack_trace_free (_fun jit_stack_trace_t -> _void))

;;jit-function.h - complete
(define JIT_OPTLEVEL_NONE	0)
(define JIT_OPTLEVEL_NORMAL	1)

(define-jit jit_function_create
  (_fun jit_context_t jit_type_t -> jit_function_t))
(define-jit jit_function_create_nested
  (_fun jit_context_t jit_type_t jit_function_t -> jit_function_t))
(define-jit jit_function_abandon (_fun jit_function_t -> _void))
(define-jit jit_function_get_context (_fun jit_function_t -> jit_context_t))
(define-jit jit_function_get_signature (_fun jit_function_t -> jit_type_t))
(define-jit jit_function_set_meta
  (_fun jit_function_t _int (pointer-to _void) jit_meta_free_func _int ->
        _int))
(define-jit jit_function_get_meta
  (_fun jit_function_t _int -> (pointer-to _void)))
(define-jit jit_function_free_meta (_fun jit_function_t _int -> _void))
(define-jit jit_function_next
  (_fun jit_context_t jit_function_t -> jit_function_t))
(define-jit jit_function_previous
  (_fun jit_context_t jit_function_t -> jit_function_t))
(define-jit jit_function_get_entry (_fun jit_function_t -> jit_block_t))
(define-jit jit_function_get_current (_fun jit_function_t -> jit_block_t))
(define-jit jit_function_get_nested_parent
  (_fun jit_function_t -> jit_function_t))
(define-jit jit_function_compile (_fun jit_function_t -> _int))
(define-jit jit_function_is_compiled (_fun jit_function_t -> _int))
(define-jit jit_function_set_recompilable (_fun jit_function_t -> _void))
(define-jit jit_function_clear_recompilable (_fun jit_function_t -> _void))
(define-jit jit_function_is_recompilable (_fun jit_function_t -> _int))
(define-jit jit_function_compile_entry
  (_fun jit_function_t (pointer-to _void) -> _int))
(define-jit jit_function_setup_entry
  (_fun jit_function_t (pointer-to _void) -> _void))
(define-jit jit_function_to_closure
  (_fun jit_function_t -> (pointer-to _void)))
(define-jit jit_function_from_closure
  (_fun jit_context_t (pointer-to _void) -> jit_function_t))
(define-jit jit_function_from_pc
  (_fun jit_context_t (pointer-to _void) (pointer-to _void) ->
   jit_function_t))
(define-jit jit_function_to_vtable_pointer
  (_fun jit_function_t -> (pointer-to _void)))
(define-jit jit_function_from_vtable_pointer
  (_fun jit_context_t (pointer-to _void) -> jit_function_t))
(define-jit jit_function_set_on_demand_compiler
  (_fun jit_function_t jit_on_demand_func -> _void))
(define-jit jit_function_get_on_demand_compiler
  (_fun jit_function_t -> jit_on_demand_func))
(define-jit jit_function_apply
  (_fun jit_function_t (pointer-to _void) (pointer-to _void) -> _int))
(define-jit jit_function_apply_vararg
  (_fun jit_function_t jit_type_t (pointer-to _void) (pointer-to _void) ->
   _int))
(define-jit jit_function_set_optimization_level
  (_fun jit_function_t _uint -> _void))
(define-jit jit_function_get_optimization_level
  (_fun jit_function_t -> _uint))
(define-jit jit_function_get_max_optimization_level (_fun -> _uint))
(define-jit jit_function_reserve_label (_fun jit_function_t -> jit_label_t))
(define-jit jit_function_labels_equal
  (_fun jit_function_t jit_label_t jit_label_t -> _int))

;;jit-init.h - complete
(define-jit jit_init (_fun -> _void))
(define-jit jit_uses_interpreter (_fun -> _int))
(define-jit jit_supports_threads (_fun -> _int))
(define-jit jit_supports_virtual_memory (_fun -> _int))
(define-jit jit_supports_closures (_fun -> _int))
(define-jit jit_get_closure_size (_fun -> _uint))
(define-jit jit_get_closure_alignment (_fun -> _uint))
(define-jit jit_get_trampoline_size (_fun -> _uint))
(define-jit jit_get_trampoline_alignment (_fun -> _uint))

;;jit-insn.h - almost complete
(define jit_intrinsic_descr_t (make-cstruct-type (list jit_type_t jit_type_t jit_type_t jit_type_t)))
(define jit_insn_iter_t (make-cstruct-type (list jit_block_t _int)))

;; TODO
;; #define	JIT_CALL_NOTHROW		(1 << 0)
;; #define	JIT_CALL_NORETURN		(1 << 1)
;; #define	JIT_CALL_TAIL			(1 << 2)

(define-jit jit_insn_get_opcode (_fun jit_insn_t -> _int))
(define-jit jit_insn_get_dest (_fun jit_insn_t -> jit_value_t))
(define-jit jit_insn_get_value1 (_fun jit_insn_t -> jit_value_t))
(define-jit jit_insn_get_value2 (_fun jit_insn_t -> jit_value_t))
(define-jit jit_insn_get_label (_fun jit_insn_t -> jit_label_t))
(define-jit jit_insn_get_function (_fun jit_insn_t -> jit_function_t))
(define-jit jit_insn_get_native (_fun jit_insn_t -> (pointer-to _void)))
(define-jit jit_insn_get_name (_fun jit_insn_t -> (pointer-to char)))
(define-jit jit_insn_get_signature (_fun jit_insn_t -> jit_type_t))
(define-jit jit_insn_dest_is_value (_fun jit_insn_t -> _int))
(define-jit jit_insn_label
  (_fun jit_function_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_label_tight
  (_fun jit_function_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_new_block (_fun jit_function_t -> _int))
(define-jit jit_insn_load (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_dup (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_load_small
  (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_store
  (_fun jit_function_t jit_value_t jit_value_t -> _int))
(define-jit jit_insn_load_relative
  (_fun jit_function_t jit_value_t jit_nint jit_type_t -> jit_value_t))
(define-jit jit_insn_store_relative
  (_fun jit_function_t jit_value_t jit_nint jit_value_t -> _int))
(define-jit jit_insn_add_relative
  (_fun jit_function_t jit_value_t jit_nint -> jit_value_t))
(define-jit jit_insn_load_elem
  (_fun jit_function_t jit_value_t jit_value_t jit_type_t -> jit_value_t))
(define-jit jit_insn_load_elem_address
  (_fun jit_function_t jit_value_t jit_value_t jit_type_t -> jit_value_t))
(define-jit jit_insn_store_elem
  (_fun jit_function_t jit_value_t jit_value_t jit_value_t -> _int))
(define-jit jit_insn_check_null (_fun jit_function_t jit_value_t -> _int))
(define-jit jit_insn_nop (_fun jit_function_t -> _int))
(define-jit jit_insn_add
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_add_ovf
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sub
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sub_ovf
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_mul
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_mul_ovf
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_div
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_rem
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_rem_ieee
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_neg (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_and
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_or
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_xor
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_not (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_shl
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_shr
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_ushr
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sshr
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_eq
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_ne
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_lt
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_le
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_gt
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_ge
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_cmpl
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_cmpg
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_to_bool
  (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_to_not_bool
  (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_acos (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_asin (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_atan (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_atan2
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_ceil (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_cos (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_cosh (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_exp (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_floor (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_log (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_log10 (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_pow
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_rint (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_round (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sin (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sinh (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sqrt (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_tan (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_tanh (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_trunc (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_is_nan (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_is_finite
  (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_is_inf (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_abs (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_min
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_max
  (_fun jit_function_t jit_value_t jit_value_t -> jit_value_t))
(define-jit jit_insn_sign (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_branch
  (_fun jit_function_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_branch_if
  (_fun jit_function_t jit_value_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_branch_if_not
  (_fun jit_function_t jit_value_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_jump_table
  (_fun jit_function_t jit_value_t (pointer-to jit_label_t) _uint -> _int))
(define-jit jit_insn_address_of
  (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_address_of_label
  (_fun jit_function_t (pointer-to jit_label_t) -> jit_value_t))
(define-jit jit_insn_convert
  (_fun jit_function_t jit_value_t jit_type_t _int -> jit_value_t))
(define-jit jit_insn_call
  (_fun jit_function_t (pointer-to char) jit_function_t jit_type_t
        (pointer-to jit_value_t) _uint _int ->
        jit_value_t))
(define-jit jit_insn_call_indirect
  (_fun jit_function_t jit_value_t jit_type_t (pointer-to jit_value_t)
        _uint _int ->
        jit_value_t))
(define-jit jit_insn_call_indirect_vtable
  (_fun jit_function_t jit_value_t jit_type_t (pointer-to jit_value_t)
        _uint _int ->
        jit_value_t))
(define-jit jit_insn_call_native
  (_fun
   jit_function_t
   (pointer-to char)
   (pointer-to _void)
   jit_type_t
   (pointer-to jit_value_t)
   _uint
   _int
   ->
   jit_value_t))
(define-jit jit_insn_call_intrinsic
  (_fun
   jit_function_t
   (pointer-to char)
   (pointer-to _void)
   (pointer-to jit_intrinsic_descr_t)
   jit_value_t
   jit_value_t
   ->
   jit_value_t))
(define-jit jit_insn_incoming_reg
  (_fun jit_function_t jit_value_t _int -> _int))
(define-jit jit_insn_incoming_frame_posn
  (_fun jit_function_t jit_value_t jit_nint -> _int))
(define-jit jit_insn_outgoing_reg
  (_fun jit_function_t jit_value_t _int -> _int))
(define-jit jit_insn_outgoing_frame_posn
  (_fun jit_function_t jit_value_t jit_nint -> _int))
(define-jit jit_insn_return_reg
  (_fun jit_function_t jit_value_t _int -> _int))
(define-jit jit_insn_setup_for_nested
  (_fun jit_function_t _int _int -> _int))
(define-jit jit_insn_flush_struct (_fun jit_function_t jit_value_t -> _int))
(define-jit jit_insn_import
  (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_push (_fun jit_function_t jit_value_t -> _int))
(define-jit jit_insn_push_ptr
  (_fun jit_function_t jit_value_t jit_type_t -> _int))
(define-jit jit_insn_set_param
  (_fun jit_function_t jit_value_t jit_nint -> _int))
(define-jit jit_insn_set_param_ptr
  (_fun jit_function_t jit_value_t jit_type_t jit_nint -> _int))
(define-jit jit_insn_push_return_area_ptr (_fun jit_function_t -> _int))
(define-jit jit_insn_pop_stack (_fun jit_function_t jit_nint -> _int))
(define-jit jit_insn_defer_pop_stack (_fun jit_function_t jit_nint -> _int))
(define-jit jit_insn_flush_defer_pop (_fun jit_function_t jit_nint -> _int))
(define-jit jit_insn_return (_fun jit_function_t jit_value_t -> _int))
(define-jit jit_insn_return_ptr
  (_fun jit_function_t jit_value_t jit_type_t -> _int))
(define-jit jit_insn_default_return (_fun jit_function_t -> _int))
(define-jit jit_insn_throw (_fun jit_function_t jit_value_t -> _int))
(define-jit jit_insn_get_call_stack (_fun jit_function_t -> jit_value_t))
(define-jit jit_insn_thrown_exception (_fun jit_function_t -> jit_value_t))
(define-jit jit_insn_uses_catcher (_fun jit_function_t -> _int))
(define-jit jit_insn_start_catcher (_fun jit_function_t -> jit_value_t))
(define-jit jit_insn_branch_if_pc_not_in_range
  (_fun
   jit_function_t
   jit_label_t
   jit_label_t
   (pointer-to jit_label_t)
   ->
   _int))
(define-jit jit_insn_rethrow_unhandled (_fun jit_function_t -> _int))
(define-jit jit_insn_start_finally
  (_fun jit_function_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_return_from_finally (_fun jit_function_t -> _int))
(define-jit jit_insn_call_finally
  (_fun jit_function_t (pointer-to jit_label_t) -> _int))
(define-jit jit_insn_start_filter
  (_fun jit_function_t (pointer-to jit_label_t) jit_type_t -> jit_value_t))
(define-jit jit_insn_return_from_filter
  (_fun jit_function_t jit_value_t -> _int))
(define-jit jit_insn_call_filter
  (_fun
   jit_function_t
   (pointer-to jit_label_t)
   jit_value_t
   jit_type_t
   ->
   jit_value_t))
(define-jit jit_insn_memcpy
  (_fun jit_function_t jit_value_t jit_value_t jit_value_t -> _int))
(define-jit jit_insn_memmove
  (_fun jit_function_t jit_value_t jit_value_t jit_value_t -> _int))
(define-jit jit_insn_memset
  (_fun jit_function_t jit_value_t jit_value_t jit_value_t -> _int))
(define-jit jit_insn_alloca (_fun jit_function_t jit_value_t -> jit_value_t))
(define-jit jit_insn_move_blocks_to_end
  (_fun jit_function_t jit_label_t jit_label_t -> _int))
(define-jit jit_insn_move_blocks_to_start
  (_fun jit_function_t jit_label_t jit_label_t -> _int))
(define-jit jit_insn_mark_offset (_fun jit_function_t jit_int -> _int))
(define-jit jit_insn_mark_breakpoint
  (_fun jit_function_t jit_nint jit_nint -> _int))
(define-jit jit_insn_mark_breakpoint_variable
  (_fun jit_function_t jit_value_t jit_value_t -> _int))
(define-jit jit_insn_iter_init
  (_fun (pointer-to jit_insn_iter_t) jit_block_t -> _void))
(define-jit jit_insn_iter_init_last
  (_fun (pointer-to jit_insn_iter_t) jit_block_t -> _void))
(define-jit jit_insn_iter_next
  (_fun (pointer-to jit_insn_iter_t) -> jit_insn_t))
(define-jit jit_insn_iter_previous
  (_fun (pointer-to jit_insn_iter_t) -> jit_insn_t))


;;jit-intrinsic.h - TODO

;;jit-meta.h - complete
(define jit_meta_t _pointer)
(define-jit jit_meta_set
  (_fun (pointer-to jit_meta_t) _int (pointer-to _void) jit_meta_free_func
   jit_function_t ->
   _int))
(define-jit jit_meta_get (_fun jit_meta_t _int -> (pointer-to _void)))
(define-jit jit_meta_free (_fun (pointer-to jit_meta_t) _int -> _void))
(define-jit jit_meta_destroy (_fun (pointer-to jit_meta_t) -> _void))

;;jit-objmodel.h - complete

(define jit_objmodel_t _pointer)
(define jitom_class_t _pointer)
(define jitom_field_t _pointer)
(define jitom_method_t _pointer)

(define	JITOM_MODIFIER_ACCESS_MASK				#x0007)
(define	JITOM_MODIFIER_PUBLIC					#x0000)
(define	JITOM_MODIFIER_PRIVATE					#x0001)
(define	JITOM_MODIFIER_PROTECTED				#x0002)
(define	JITOM_MODIFIER_PACKAGE					#x0003)
(define	JITOM_MODIFIER_PACKAGE_OR_PROTECTED			#x0004)
(define	JITOM_MODIFIER_PACKAGE_AND_PROTECTED			#x0005)
(define	JITOM_MODIFIER_OTHER1					#x0006)
(define	JITOM_MODIFIER_OTHER2					#x0007)
(define	JITOM_MODIFIER_STATIC					#x0008)
(define	JITOM_MODIFIER_VIRTUAL					#x0010)
(define	JITOM_MODIFIER_NEW_SLOT					#x0020)
(define	JITOM_MODIFIER_ABSTRACT					#x0040)
(define	JITOM_MODIFIER_LITERAL					#x0080)
(define	JITOM_MODIFIER_CTOR					#x0100)
(define	JITOM_MODIFIER_STATIC_CTOR				#x0200)
(define	JITOM_MODIFIER_DTOR					#x0400)
(define	JITOM_MODIFIER_INTERFACE				#x0800)
(define	JITOM_MODIFIER_VALUE					#x1000)
(define	JITOM_MODIFIER_FINAL					#x2000)
(define	JITOM_MODIFIER_DELETE					#x4000)
(define	JITOM_MODIFIER_REFERENCE_COUNTED			#x8000)

(define	JITOM_TYPETAG_CLASS		11000)
(define	JITOM_TYPETAG_VALUE		11001)

;; Operations on objec models
(define-jit jitom_destroy_model (_fun jit_objmodel_t -> _void))
(define-jit jitom_get_class_by_name
  (_fun jit_objmodel_t (pointer-to char) -> jitom_class_t))

;; Operations on object model classes.
(define-jit jitom_class_get_name
  (_fun jit_objmodel_t jitom_class_t -> (pointer-to char)))
(define-jit jitom_class_get_modifiers
  (_fun jit_objmodel_t jitom_class_t -> _int))
(define-jit jitom_class_get_type
  (_fun jit_objmodel_t jitom_class_t -> jit_type_t))
(define-jit jitom_class_get_value_type
  (_fun jit_objmodel_t jitom_class_t -> jit_type_t))
(define-jit jitom_class_get_primary_super
  (_fun jit_objmodel_t jitom_class_t -> jitom_class_t))
(define-jit jitom_class_get_all_supers
  (_fun
   jit_objmodel_t
   jitom_class_t
   (pointer-to _uint)
   ->
   (pointer-to jitom_class_t)))
(define-jit jitom_class_get_interfaces
  (_fun
   jit_objmodel_t
   jitom_class_t
   (pointer-to _uint)
   ->
   (pointer-to jitom_class_t)))
(define-jit jitom_class_get_fields
  (_fun
   jit_objmodel_t
   jitom_class_t
   (pointer-to _uint)
   ->
   (pointer-to jitom_field_t)))
(define-jit jitom_class_get_methods
  (_fun
   jit_objmodel_t
   jitom_class_t
   (pointer-to _uint)
   ->
   (pointer-to jitom_method_t)))
(define-jit jitom_class_new
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_method_t
   jit_function_t
   (pointer-to jit_value_t)
   _uint
   _int
   ->
   jit_value_t))
(define-jit jitom_class_new_value
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_method_t
   jit_function_t
   (pointer-to jit_value_t)
   _uint
   _int
   ->
   jit_value_t))
(define-jit jitom_class_delete
  (_fun jit_objmodel_t jitom_class_t jit_value_t -> _int))
(define-jit jitom_class_add_ref
  (_fun jit_objmodel_t jitom_class_t jit_value_t -> _int))

;; Operations on object model fields.
(define-jit jitom_field_get_name
  (_fun jit_objmodel_t jitom_class_t jitom_field_t -> (pointer-to char)))
(define-jit jitom_field_get_type
  (_fun jit_objmodel_t jitom_class_t jitom_field_t -> jit_type_t))
(define-jit jitom_field_get_modifiers
  (_fun jit_objmodel_t jitom_class_t jitom_field_t -> _int))
(define-jit jitom_field_load
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_field_t
   jit_function_t
   jit_value_t
   ->
   jit_value_t))
(define-jit jitom_field_load_address
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_field_t
   jit_function_t
   jit_value_t
   ->
   jit_value_t))
(define-jit jitom_field_store
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_field_t
   jit_function_t
   jit_value_t
   jit_value_t
   ->
   _int))

;; Operations on object model methods.
(define-jit jitom_method_get_name
  (_fun jit_objmodel_t jitom_class_t jitom_method_t -> (pointer-to char)))
(define-jit jitom_method_get_type
  (_fun jit_objmodel_t jitom_class_t jitom_method_t -> jit_type_t))
(define-jit jitom_method_get_modifiers
  (_fun jit_objmodel_t jitom_class_t jitom_method_t -> _int))
(define-jit jitom_method_invoke
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_method_t
   jit_function_t
   (pointer-to jit_value_t)
   _uint
   _int
   ->
   jit_value_t))
(define-jit jitom_method_invoke_virtual
  (_fun
   jit_objmodel_t
   jitom_class_t
   jitom_method_t
   jit_function_t
   (pointer-to jit_value_t)
   _uint
   _int
   ->
   jit_value_t))

;; Manipulate types that represent objects and inline values.
(define-jit jitom_type_tag_as_class
  (_fun jit_type_t jit_objmodel_t jitom_class_t _int -> jit_type_t))
(define-jit jitom_type_tag_as_value
  (_fun jit_type_t jit_objmodel_t jitom_class_t _int -> jit_type_t))
(define-jit jitom_type_is_class (_fun jit_type_t -> _int))
(define-jit jitom_type_is_value (_fun jit_type_t -> _int))
(define-jit jitom_type_get_model (_fun jit_type_t -> jit_objmodel_t))
(define-jit jitom_type_get_class (_fun jit_type_t -> jitom_class_t))

;;jit-opcode.h - TODO


;;jit-type.h - complete
(define-jit jit_type_void jit_type_t)
(define-jit jit_type_sbyte jit_type_t)
(define-jit jit_type_ubyte jit_type_t)
(define-jit jit_type_short jit_type_t)
(define-jit jit_type_ushort jit_type_t)
(define-jit jit_type_int jit_type_t)
(define-jit jit_type_uint jit_type_t)
(define-jit jit_type_nint jit_type_t)
(define-jit jit_type_nuint jit_type_t)
(define-jit jit_type_long jit_type_t)
(define-jit jit_type_ulong jit_type_t)
(define-jit jit_type_float32 jit_type_t)
(define-jit jit_type_float64 jit_type_t)
(define-jit jit_type_nfloat jit_type_t)
(define-jit jit_type_void_ptr jit_type_t)

(define jit_abi_t (_enum '(jit_abi_cdecl jit_abi_vararg jit_abi_stdcall jit_abi_fastcall)))

;;;using autoffi
(define-jit jit_type_copy (_fun jit_type_t -> jit_type_t))
(define-jit jit_type_free (_fun jit_type_t -> _void))
(define-jit
  jit_type_create_struct
  (_fun (pointer-to jit_type_t) _uint _int -> jit_type_t))
(define-jit
  jit_type_create_union
  (_fun (pointer-to jit_type_t) _uint _int -> jit_type_t))
(define-jit
  jit_type_create_signature
  (_fun jit_abi_t jit_type_t (pointer-to jit_type_t) _uint _int ->
   jit_type_t))
(define-jit jit_type_create_pointer (_fun jit_type_t _int -> jit_type_t))
(define-jit
  jit_type_create_tagged
  (_fun jit_type_t _int (pointer-to _void) jit_meta_free_func _int ->
   jit_type_t))
(define-jit
  jit_type_set_names
  (_fun jit_type_t (pointer-to char) _uint -> _int))
(define-jit
  jit_type_set_size_and_alignment
  (_fun jit_type_t jit_nint jit_nint -> _void))
(define-jit jit_type_set_offset (_fun jit_type_t _uint jit_nuint -> _void))
(define-jit jit_type_get_kind (_fun jit_type_t -> _int))
(define-jit jit_type_get_size (_fun jit_type_t -> jit_nuint))
(define-jit jit_type_get_alignment (_fun jit_type_t -> jit_nuint))
(define-jit jit_type_num_fields (_fun jit_type_t -> _uint))
(define-jit jit_type_get_field (_fun jit_type_t _uint -> jit_type_t))
(define-jit jit_type_get_offset (_fun jit_type_t _uint -> jit_nuint))
(define-jit jit_type_get_name (_fun jit_type_t _uint -> (pointer-to char)))
(define-jit jit_type_find_name (_fun jit_type_t (pointer-to char) -> _uint))
(define-jit jit_type_num_params (_fun jit_type_t -> _uint))
(define-jit jit_type_get_return (_fun jit_type_t -> jit_type_t))
(define-jit jit_type_get_param (_fun jit_type_t _uint -> jit_type_t))
(define-jit jit_type_get_abi (_fun jit_type_t -> jit_abi_t))
(define-jit jit_type_get_ref (_fun jit_type_t -> jit_type_t))
(define-jit jit_type_get_tagged_type (_fun jit_type_t -> jit_type_t))
(define-jit
  jit_type_set_tagged_type
  (_fun jit_type_t jit_type_t _int -> _void))
(define-jit jit_type_get_tagged_kind (_fun jit_type_t -> _int))
(define-jit jit_type_get_tagged_data (_fun jit_type_t -> (pointer-to _void)))
(define-jit
  jit_type_set_tagged_data
  (_fun jit_type_t (pointer-to _void) jit_meta_free_func -> _void))
(define-jit jit_type_is_primitive (_fun jit_type_t -> _int))
(define-jit jit_type_is_struct (_fun jit_type_t -> _int))
(define-jit jit_type_is_union (_fun jit_type_t -> _int))
(define-jit jit_type_is_signature (_fun jit_type_t -> _int))
(define-jit jit_type_is_pointer (_fun jit_type_t -> _int))
(define-jit jit_type_is_tagged (_fun jit_type_t -> _int))
(define-jit jit_type_best_alignment (_fun -> jit_nuint))
(define-jit jit_type_normalize (_fun jit_type_t -> jit_type_t))
(define-jit jit_type_remove_tags (_fun jit_type_t -> jit_type_t))
(define-jit jit_type_promote_int (_fun jit_type_t -> jit_type_t))
(define-jit jit_type_return_via_pointer (_fun jit_type_t -> _int))
(define-jit jit_type_has_tag (_fun jit_type_t _int -> _int))

;;jit-unwind.h TODO

;;jit-util.h - almost complete
(define-jit jit_malloc (_fun _uint -> (pointer-to _void)))
(define-jit jit_calloc (_fun _uint _uint -> (pointer-to _void)))
(define-jit jit_realloc
  (_fun (pointer-to _void) _uint -> (pointer-to _void)))
(define-jit jit_free (_fun (pointer-to _void) -> _void))
(define-jit jit_memset
  (_fun (pointer-to _void) _int _uint -> (pointer-to _void)))
(define-jit jit_memcpy
  (_fun (pointer-to _void) (pointer-to _void) _uint -> (pointer-to _void)))
(define-jit jit_memmove
  (_fun (pointer-to _void) (pointer-to _void) _uint -> (pointer-to _void)))
(define-jit jit_memcmp
  (_fun (pointer-to _void) (pointer-to _void) _uint -> _int))
(define-jit jit_memchr
  (_fun (pointer-to _void) _int _uint -> (pointer-to _void)))
(define-jit jit_strlen (_fun (pointer-to char) -> _uint))
(define-jit jit_strcpy
  (_fun (pointer-to char) (pointer-to char) -> (pointer-to char)))
(define-jit jit_strcat
  (_fun (pointer-to char) (pointer-to char) -> (pointer-to char)))
(define-jit jit_strncpy
  (_fun (pointer-to char) (pointer-to char) _uint -> (pointer-to char)))
(define-jit jit_strdup (_fun (pointer-to char) -> (pointer-to char)))
(define-jit jit_strndup (_fun (pointer-to char) _uint -> (pointer-to char)))
(define-jit jit_strcmp (_fun (pointer-to char) (pointer-to char) -> _int))
(define-jit jit_strncmp
  (_fun (pointer-to char) (pointer-to char) _uint -> _int))
(define-jit jit_stricmp (_fun (pointer-to char) (pointer-to char) -> _int))
(define-jit jit_strnicmp
  (_fun (pointer-to char) (pointer-to char) _uint -> _int))
(define-jit jit_strchr (_fun (pointer-to char) _int -> (pointer-to char)))
(define-jit jit_strrchr (_fun (pointer-to char) _int -> (pointer-to char)))
;; int jit_sprintf(char *str, const char *format, ...) ;
;; int jit_snprintf
;; 	(char *str, unsigned int len, const char *format, ...) ;


;;jit-value.h - almost complete

(define jit_constant_t _pointer)
;; TODO racket error **stack smashing detected**
;; (define jit_constant_t
;;   (_list-struct
;;    jit_type_t
;;    (_union _pointer
;;            jit_int
;;            jit_uint
;;            jit_nint
;;            jit_nuint
;;            jit_long
;;            jit_ulong
;;            jit_float32
;;            jit_float64
;;            jit_nfloat)
;;    ))

(define-jit jit_value_create (_fun jit_function_t jit_type_t -> jit_value_t))
(define-jit jit_value_create_nint_constant
  (_fun jit_function_t jit_type_t jit_nint -> jit_value_t))
(define-jit jit_value_create_long_constant
  (_fun jit_function_t jit_type_t jit_long -> jit_value_t))
(define-jit jit_value_create_float32_constant
  (_fun jit_function_t jit_type_t jit_float32 -> jit_value_t))
(define-jit jit_value_create_float64_constant
  (_fun jit_function_t jit_type_t jit_float64 -> jit_value_t))
(define-jit jit_value_create_nfloat_constant
  (_fun jit_function_t jit_type_t jit_nfloat -> jit_value_t))
(define-jit jit_value_create_constant
  (_fun jit_function_t (pointer-to jit_constant_t) -> jit_value_t))
(define-jit jit_value_get_param (_fun jit_function_t _uint -> jit_value_t))
(define-jit jit_value_get_struct_pointer
  (_fun jit_function_t -> jit_value_t))
(define-jit jit_value_is_temporary (_fun jit_value_t -> _int))
(define-jit jit_value_is_local (_fun jit_value_t -> _int))
(define-jit jit_value_is_constant (_fun jit_value_t -> _int))
(define-jit jit_value_is_parameter (_fun jit_value_t -> _int))
(define-jit jit_value_ref (_fun jit_function_t jit_value_t -> _void))
(define-jit jit_value_set_volatile (_fun jit_value_t -> _void))
(define-jit jit_value_is_volatile (_fun jit_value_t -> _int))
(define-jit jit_value_set_addressable (_fun jit_value_t -> _void))
(define-jit jit_value_is_addressable (_fun jit_value_t -> _int))
(define-jit jit_value_get_type (_fun jit_value_t -> jit_type_t))
(define-jit jit_value_get_function (_fun jit_value_t -> jit_function_t))
(define-jit jit_value_get_block (_fun jit_value_t -> jit_block_t))
(define-jit jit_value_get_context (_fun jit_value_t -> jit_context_t))
(define-jit jit_value_get_constant (_fun jit_value_t -> jit_constant_t))
(define-jit jit_value_get_nint_constant (_fun jit_value_t -> jit_nint))
(define-jit jit_value_get_long_constant (_fun jit_value_t -> jit_long))
(define-jit jit_value_get_float32_constant (_fun jit_value_t -> jit_float32))
(define-jit jit_value_get_float64_constant (_fun jit_value_t -> jit_float64))
(define-jit jit_value_get_nfloat_constant (_fun jit_value_t -> jit_nfloat))
(define-jit jit_value_is_true (_fun jit_value_t -> _int))
(define-jit jit_constant_convert
  (_fun (pointer-to jit_constant_t) (pointer-to jit_constant_t) jit_type_t _int -> _int))

;;jit-vmem.h - complete
(define jit_prot_t
  (_enum '(JIT_PROT_NONE
           JIT_PROT_READ
           JIT_PROT_READ_WRITE
           JIT_PROT_EXEC_READ
           JIT_PROT_EXEC_READ_WRITE)))

(define-jit jit_vmem_init (_fun -> _void))
(define-jit jit_vmem_page_size (_fun -> jit_uint))
(define-jit jit_vmem_round_up (_fun jit_nuint -> jit_nuint))
(define-jit jit_vmem_round_down (_fun jit_nuint -> jit_nuint))
(define-jit jit_vmem_reserve (_fun jit_uint -> (pointer-to _void)))
(define-jit jit_vmem_reserve_committed
  (_fun jit_uint jit_prot_t -> (pointer-to _void)))
(define-jit jit_vmem_release (_fun (pointer-to _void) jit_uint -> _int))
(define-jit jit_vmem_commit
  (_fun (pointer-to _void) jit_uint jit_prot_t -> _int))
(define-jit jit_vmem_decommit (_fun (pointer-to _void) jit_uint -> _int))
(define-jit jit_vmem_protect
  (_fun (pointer-to _void) jit_uint jit_prot_t -> _int))


;;jit-walk.h - complete
;typedef struct { void * volatile mark; } jit_crawl_mark_t;
(define jit_crawl_mark_t _void)
(define-jit _jit_get_frame_address
  (_fun (pointer-to _void) _uint -> (pointer-to _void)))
(define-jit _jit_get_next_frame_address
  (_fun (pointer-to _void) -> (pointer-to _void)))
(define-jit _jit_get_return_address
  (_fun
   (pointer-to _void)
   (pointer-to _void)
   (pointer-to _void)
   ->
   (pointer-to _void)))
(define-jit jit_frame_contains_crawl_mark
  (_fun (pointer-to _void) (pointer-to jit_crawl_mark_t) -> _int))
