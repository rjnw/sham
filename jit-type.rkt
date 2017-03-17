#lang racket

(require ffi/unsafe)
(require "llvm/ffi/all.rkt")
(require "jit-env.rkt")
(provide (all-defined-out))

(struct type-prim (racket jit) #:prefab)

;skeleton of types
(struct type-internal ())
(struct type-ref (to) #:prefab) ;to: env-type
(struct type-struct (names types) #:prefab)
(struct type-function (args ret) #:prefab)
(struct type-pointer (to) #:prefab)

;;returns one of the skeleton
(define (create-type-skel typ env)
  (match typ
    [`(struct (,ids : ,types) ...) (type-struct ids (map (curryr env-lookup env) types))]
    [`(pointer ,t) (type-pointer (env-lookup t env))]
    [`(,args ... -> ,ret)
     (type-function (map (curryr env-lookup env) args) (env-lookup ret env))]
    [symbol? (type-ref (env-lookup typ env))]))

(define (compile-type-declaration t env)
  (match t
    [`(define-type ,id ,typ) (create-type-skel typ env)]))

;;returns one of env-type object
(define (compile-type-definition t-obj env)
  (define (env-lookup-prim types)
    (map env-type-prim (map (curryr env-lookup env) types)))
  (env-type
   t-obj
   (match t-obj
     [(type-ref t) (env-type-prim t)]
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
    (LLVMFunctionType (type-prim-jit ret) (map type-prim-jit args) #f))
  (type-prim (create-racket-function-type args ret)
             (create-jit-function-type args ret)))

(define (create-struct-type types)
  (define (create-racket-struct-type types)
    (make-cstruct-type (map type-prim-racket types)))
  (define (create-jit-struct-type types)
    (LLVMStructType (map type-prim-jit types) #t))
  (type-prim (create-racket-struct-type types)
             (create-jit-struct-type types)))

(define (create-pointer-type type)
  (define (create-racket-pointer-type type)
    _pointer)
  (define (create-jit-pointer-type type)
    (LLVMPointerType (type-prim-jit type) 0))
  (printf "type ~a jit-type ~a\n" type (type-prim-jit type))
  (type-prim (create-racket-pointer-type type)
             (create-jit-pointer-type type)))

;TODO add couple more pointer types for basic types
(define (register-initial-types env context)
  (define (register-types types)
    (for/fold [(env env)]
              [(t types)]
      (env-extend (first t)
                  (env-type (type-internal)
                            (type-prim (third t) (second t)))
                  env)))
  (define type-void*
    (env-type (type-pointer 'void)
              (type-prim  _pointer
                          (LLVMPointerType (LLVMVoidTypeInContext context) 0))))
  (define new-env
    (register-types
     `((i1 ,(LLVMInt1TypeInContext context) ,_uint)
       (i8 ,(LLVMInt8TypeInContext context) ,_uint8)
       (i16 ,(LLVMInt16TypeInContext context) ,_uint16)
       (i32 ,(LLVMInt32TypeInContext context) ,_uint32)
       (i64 ,(LLVMInt64TypeInContext context) ,_uint64)
       (i128 ,(LLVMInt128TypeInContext context) ,_ullong)
       (f32 ,(LLVMFloatTypeInContext context) ,_float)
       (f64 ,(LLVMDoubleTypeInContext context) ,_double)
      (void ,(LLVMVoidTypeInContext context) ,_void))))
  (env-extend 'void* type-void* new-env))



(define native-int-types (set _int _uint _sbyte _ubyte _short _ushort _long _ulong))
(define (type-native-int? envtype)
  (match envtype
    [(env-type (type-internal) (type-prim racket-type jit-type))
     (set-member? native-int-types racket-type)]
    [(env-type (type-ref t) _)
     (type-native-int? t)]
    [else #f]))

(define (type-float32? envtype)
  (match envtype
    [(env-type (type-internal) (type-prim racket-type jit-type))
     (equal? _float jit-type)]
    [(env-type (type-ref t) _)
     (type-float32? t)]
    [else #f]))

(define (racket-type-cast object from-type to-type)
  (cast object
        (type-prim-racket (env-type-prim from-type))
        (type-prim-racket (env-type-prim to-type))))


(module+ test
  (require rackunit)
  (define env0 (register-initial-types (empty-env) (LLVMGetGlobalContext)))
  (printf "env0: ~a\n" env0)
  (define env1 (env-extend 'double*
                              (create-type '(pointer f64)  env0)
                              env0))
  (define env2 (env-extend 'sp
                              (create-type '(struct (a : i32) (b : i32))  env1)
                              env1))
  (define new-env1 (env-extend 'array-real
                              (create-type '(struct (size : i32)  (data : i32)) env2) env2))
  (pretty-print new-env1)
  (pretty-display (create-type '(pointer i32) env0))
  (pretty-display (create-type '(i32 -> i32) env0))
  (pretty-display (create-type '(struct (a : i32) (b : i32)) env0))
  (define new-env (env-extend 'intref (create-type 'i32 env0) env0))
  (pretty-display (type-native-int? (env-lookup 'i32 new-env)))
  (pretty-display (type-native-int? (env-lookup 'intref new-env)))
  (pretty-display (type-native-int? (env-lookup 'void* new-env)))
  (pretty-display (type-float32? (env-lookup 'f32 new-env))))
