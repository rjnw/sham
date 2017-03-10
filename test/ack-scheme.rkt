#lang racket
(require ffi/unsafe)
(require "../libjit.rkt")

(define context (jit_context_create))
(define empty-label (jit_uint_not 0))
(define jit_type_scheme_object (jit_type_create_struct (list jit_type_short jit_type_short) 1))
(define jit_type_scheme_object_p (jit_type_create_pointer jit_type_scheme_object 1))

(define (compile-ack context)
  (jit_context_build_start context)
  (define sig (jit_type_create_signature 'jit_abi_cdecl jit_type_uint
                                         (list jit_type_uint jit_type_uint) 1))

  (define f (jit_function_create context sig))

  (define m (jit_value_get_param f 0))
  (define n (jit_value_get_param f 1))

  (define c0 (jit_value_create_constant fobj (make-jit-constant-t jit_type_schem_object_p
                                                                  (cast (cast 0 _int _scheme)
                                                                        _scheme _pointer))))
  (define c1 (jit_value_create_constant fobj (make-jit-constant-t jit_type_schem_object_p
                                                                  (cast (cast 1 _int _scheme)
                                                                        _scheme _pointer))))

  (define temp1 (jit_insn_eq f m c0))
  (define label-if-1 (jit_insn_branch_if_not f temp1 empty-label))
  (jit_insn_return f (jit_insn_add f n c1))

  (jit_insn_label f label-if-1)
  
  (define temp2 (jit_insn_eq f n c0))
  (define label-if-2 (jit_insn_branch_if_not f temp2 empty-label))
  (jit_insn_return f
                   (jit_insn_call f "ack" f #f
                                  (list (jit_insn_sub f m c1) c1)
                                   0))

  (jit_insn_label f label-if-2)
  (jit_insn_return f (jit_insn_call f "ack" f #f
                                    (list
                                     (jit_insn_sub f m c1)
                                     (jit_insn_call f "ack" f #f
                                                    (list m
                                                          (jit_insn_sub f n c1))
                                                    0))
                                    0))
  (jit_function_compile f)
  (jit_context_build_end context)
  f)


(define ack (cast (jit_function_to_closure (compile-ack context))
                 _pointer
                 (_fun jit_uint jit_uint -> jit_uint)))

(require racket/unsafe/ops)
(time (ack 3 9))
(define ackn (lambda (m n)
               (if (zero? m)
                   (unsafe-fx+ n 1)
                   (if (unsafe-fx= n 0)
                       (ack (unsafe-fx- m 1) 1)
                       (ack (unsafe-fx- m 1) (ack m (unsafe-fx- n 1)))))))
(time (ackn 3 9))


