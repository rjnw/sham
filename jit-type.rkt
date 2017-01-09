#lang racket
(require ffi/unsafe)
(require "libjit.rkt")
(require "jit-env.rkt")
(provide (all-defined-out))

;; type ::= prims | (struct (id : type) ...) | (pointer type) | (union type ...) | (-> type ... : type)
(struct type-prim (racket jit) #:prefab)

(struct type-internal ())
(struct type-ref (to) #:prefab)
(struct type-struct (names types) #:prefab)
(struct type-function (args ret) #:prefab)
(struct type-pointer (to) #:prefab)

(define (create-type-skel typ env)
  (match typ
    [x #:when (symbol? typ) (type-ref (env-lookup typ env))]
    [`(struct (,ids : ,types) ...) (type-struct ids (map (curryr env-lookup env) types))]
    [`(pointer ,t) (type-pointer (env-lookup t env))]
    [`(,args ... -> ,ret)
     (type-function (map (curryr env-lookup env) args) (env-lookup ret env))]))

(define (compile-type-declaration t env)
  (match t
    [`(define-type ,id ,typ) (create-type-skel typ env)]))

(define (compile-type-definition t-obj env)
  (define (env-lookup-prim types)
    (map env-type-prim (map (curryr env-lookup env) types)))
  (env-type
   t-obj
   (match t-obj
     [(type-ref t) t]
     [(type-struct names types)
      (create-struct-type (map env-type-prim types))]
     [(type-function args ret)
      (create-function-type (map env-type-prim args) (env-type-prim ret))]
     [(type-pointer to)
      (create-pointer-type (env-type-prim to))])))

(define (create-type type-ast env)
  (compile-type-definition (create-type-skel type-ast env) env))

;input type-prim
(define (create-function-type args ret)
  (define (create-racket-function-type args ret)
    (_cprocedure (map type-prim-racket args)
                 (type-prim-racket ret)))
  (define (create-jit-function-type args ret)
    (jit_type_create_signature 'jit_abi_cdecl (type-prim-jit ret) (map type-prim-jit args) 1))
  (type-prim (create-racket-function-type args ret)
             (create-jit-function-type args ret)))

(define (create-struct-type types)
  (define (create-racket-struct-type types)
    (make-cstruct-type (map type-prim-racket types)))
  (define (create-jit-struct-type types)
    (jit_type_create_struct (map type-prim-jit types) 1))
  (type-prim (create-racket-struct-type types)
             (create-jit-struct-type types)))

(define (create-pointer-type type)
  (define (create-racket-pointer-type type)
    _pointer)
  (define (create-jit-pointer-type type)
    (jit_type_create_pointer (type-prim-jit type) 1))
  (type-prim (create-racket-pointer-type type)
             (create-jit-pointer-type type)))

;TODO add couple more pointer types for basic types
(define (register-initial-types env)
  (define (register-types types)
    (for/fold [(env env)]
              [(t types)]
      (env-extend (first t)
                  (env-type (type-internal)
                            (type-prim (second t) (third t)))
                  env)))
  (define new-env
    (register-types
    `((int ,jit_int ,jit_type_int)
      (uint ,jit_uint ,jit_type_uint)
      (sbyte ,jit_sbyte ,jit_type_sbyte)
      (ubyte ,jit_ubyte ,jit_type_ubyte)
      (short ,jit_short ,jit_type_short)
      (ushort ,jit_ushort ,jit_type_ushort)
      (long ,jit_long ,jit_type_long)
      (ulong ,jit_ulong ,jit_type_ulong)
      (float32 ,jit_float32 ,jit_type_float32)
      (float64 ,jit_float64 ,jit_type_float64)
      (void ,jit_void ,jit_type_void))))
  (env-extend 'void* type-void* new-env))

(define type-void* (env-type (type-pointer 'void) (type-prim jit_ptr jit_type_void_ptr)))

(define native-int-types (set jit_type_int jit_type_uint jit_type_sbyte jit_type_ubyte jit_type_short
                              jit_type_ushort jit_type_long jit_type_ulong))
(define (type-native-int? envtype)
  (match envtype
    [(env-type (type-internal) (type-prim racket-type jit-type))
     (set-member? native-int-types jit-type)]
    [(env-type (type-ref t) _)
     (type-native-int? t)]
    [else #f]))

(define (type-float32? envtype)
  (match envtype
    [(env-type (type-internal) (type-prim racket-type jit-type))
     (equal? jit_type_float32 jit-type)]
    [(env-type (type-ref t) _)
     (type-float32? t)]
    [else #f]))

(define (racket-type-cast object from-type to-type)
  (cast object
        (type-prim-racket (env-type-prim from-type))
        (type-prim-racket (env-type-prim to-type))))

(module+ test
  (require rackunit)
  (define env (register-initial-types (empty-env)))
  (pretty-display env)
  (pretty-display (create-type '(pointer int) env))
  (pretty-display (create-type '(int -> int) env))
  (pretty-display (create-type '(struct (a : int) (b : int)) env))
  (define new-env (env-extend 'intref (create-type 'int env) env))
  (pretty-display (type-native-int? (env-lookup 'int new-env)))
  (pretty-display (type-native-int? (env-lookup 'intref new-env)))
  (pretty-display (type-native-int? (env-lookup 'void* new-env)))
  (pretty-display (type-float32? (env-lookup 'float32 new-env)))
  )
