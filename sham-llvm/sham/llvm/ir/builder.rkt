#lang racket

(require sham/llvm/ir/ast
         sham/llvm/ir/env
         sham/llvm/ir/init
         sham/llvm/ir/internals
         sham/llvm/ir/md
         sham/llvm/ffi
         ffi/unsafe)

(provide diagnose-builder
         build-llvm-module)

(define diagnose-builder (make-parameter #f))

(define (to-string s)
  (match s
    [(? symbol?) (symbol->string s)]
    [(? string?) s]
    [else (error 'sham "invalid name for llvm values ~a" s)]))

(define (build-llvm-module module-ast
                           [llvm-context (global-context)]
                           [llvm-target-triple (LLVMGetDefaultTargetTriple)]
                           [llvm-data-layout #f])
  (when (diagnose-builder)
    (define (diag-handler dinfo voidp)
      (define diag-desc (LLVMGetDiagInfoDescription dinfo))
      (eprintf "llvm-diagnose: ~a\n" (cast diag-desc _pointer _string))
      (LLVMDisposeMessage diag-desc))
    (LLVMContextSetDiagnosticHandler llvm-context diag-handler #f))
  (match-define (llvm:def:module module-info module-name module-defs) module-ast)
  (define llvm-module (LLVMModuleCreateWithNameInContext (to-string module-name) llvm-context))
  (define llvm-builder (LLVMCreateBuilderInContext llvm-context))
  (when llvm-data-layout (LLVMSetDataLayout llvm-module llvm-data-layout))
  (LLVMSetTarget llvm-module llvm-target-triple)

  ;; llvm:def LLVMModuleRef env -> env
  (define (register-define def internal-env decl-env)
    (match def
      [(llvm:def:type info name t)
       (if (llvm:ast:type:struct? t)
           (assoc-env-extend decl-env name (LLVMStructCreateNamed llvm-context (to-string name)))
           decl-env)]
      [(llvm:def:function info name args arg-types ret-type body)
       (define type (compile-type (llvm:ast:type:function arg-types ret-type) internal-env decl-env))
       (define value (LLVMAddFunction llvm-module (to-string name) type))
       (assoc-env-extend decl-env name (llvm-function value type))]
      [(llvm:def:global info name t v)
       (define type (compile-type t internal-env decl-env))
       (define value (LLVMAddGlobal llvm-module type (to-string name)))
       (LLVMSetInitializer value
                           (if v (compile-constant v internal-env decl-env) (LLVMConstNull type)))
       (assoc-env-extend decl-env name (llvm-value value type))]))

  ;; llvm:ast:type env -> LLVMTypeRef
  (define (compile-type type internal-env decl-env)
    (match type
      [(llvm:ast:type:ref _ t)
       (if (assoc-env-contains? decl-env t)
           (assoc-env-lookup decl-env t)
           (assoc-env-lookup internal-env t))]
      [(llvm:ast:type:struct _ fields)
       (LLVMStructType (map (curryr compile-type internal-env decl-env) fields) #f)]
      [(llvm:ast:type:function _ args ret)
       (LLVMFunctionType (compile-type ret internal-env decl-env)
                         (map (curryr compile-type internal-env decl-env) args)
                         #f)]
      [(llvm:ast:type:pointer _ to)
       (LLVMPointerType (compile-type to internal-env decl-env) 0)]
      [(llvm:ast:type:array _ of size)
       (LLVMArrayType (compile-type of internal-env decl-env) size)]
      [(llvm:ast:type:vector _ of size)
       (LLVMVectorType (compile-type of internal-env decl-env) size)]))

  (define (compile-constant v internal-env decl-env (value-compiler compile-constant))
    (match v
      [(llvm:ast:constant:fl md value t)
       (LLVMConstReal (compile-type t internal-env decl-env) value)]
      [(llvm:ast:constant:si md value t)
       (LLVMConstInt (compile-type t internal-env decl-env) (cast value _sint64 _uint64) #f)]
      [(llvm:ast:constant:ui md value t)
       (LLVMConstInt (compile-type t internal-env decl-env) value #f)]
      [(llvm:ast:constant:llvm md v t) v]
      [(llvm:ast:constant:basic-struct md fields)
       (LLVMConstStruct (map (curryr value-compiler internal-env decl-env) fields) #f)]
      [(llvm:ast:constant:named-struct md fields type)
       (LLVMConstNamedStruct (compile-type type internal-env decl-env)
                             (map (curryr value-compiler internal-env decl-env) fields))]
      [(llvm:ast:constant:array md vals t)
       (LLVMConstArray (compile-type t internal-env decl-env)
                       (map (curryr value-compiler internal-env decl-env) vals))]
      [(llvm:ast:constant:string md str)
       (LLVMBuildGlobalStringPtr llvm-builder str (to-string (gensym 'str)))]
      [(llvm:ast:constant:vector md vals)
       (LLVMConstVector (map (curryr value-compiler internal-env decl-env) vals))]))

  (define (compile-define def internal-env decl-env)
    (define (compile-function-definition! def name args types ret-type blocks)
      (define function (assoc-env-lookup decl-env name))
      (define function-ref (llvm-value-ref function))
      (define function-type (llvm-value-type function))
      (define block-map (make-hash))
      (define (block-ref sym) (hash-ref block-map sym))
      (define local-var-map (make-hash))
      (define (is-local-var? sym) (hash-has-key? local-var-map sym))
      (define (add-local-var! sym val) (hash-set! local-var-map sym val))
      (define (local-var-ref sym) (hash-ref local-var-map sym))
      (define target-data (LLVMCreateTargetData (LLVMGetDataLayout llvm-module)))
      (define (llvm-type t) (compile-type t internal-env decl-env))
      (define (append-block! n)
        (LLVMAppendBasicBlockInContext (LLVMGetModuleContext llvm-module)
                                       function-ref (to-string n)))
      (for ([a args]
            [i (length args)])
        (add-local-var! a (LLVMGetParam function-ref i)))
      (for ([b blocks])
        (match b
          [(llvm:ast:block md name instructions terminator)
           (hash-set! block-map name (append-block! name))]
          [else (error 'sham:llvm "invalid block in function definitions ~a" b)]))
      (define (build-block! b)
        (define (compile-value v)
          (match v
            [(? (curry assoc-env-contains? decl-env)) (assoc-env-lookup decl-env v)]
            [(? symbol?) (local-var-ref v)]
            [(? llvm:ast:constant?) (compile-constant v internal-env decl-env
                                                      (λ (v ie de) (compile-value v)))]
            [(? llvm:ast:type?) (compile-type v internal-env decl-env)]
            [else (error 'sham:llvm "unknown value ~a" v)]))
        (define (build-initial-instruction! instruction)
          (match instruction
            [(llvm:ast:instruction:op md result 'phi flags (list type vars labels))
             (define value (LLVMBuildPhi llvm-builder (compile-type type internal-env decl-env) (to-string result)))
             (LLVMAddIncoming value (map compile-value vars) (map block-ref labels))
             value]
            [else (build-instruction! instruction)]))
        (define (build-instruction! instruction)
          (define (add-function-call-md! value md) value) ;; TODO
          (match instruction
            [(llvm:ast:instruction:op md result op flags args)
             (add-local-var!
              result
              (cond
                [(assoc-env-ref internal-env op)
                 => (λ (opbuilder) (opbuilder llvm-builder flags args
                                              (curryr compile-type internal-env decl-env)
                                              compile-value
                                              (to-string result)))]
                [(assoc-env-ref decl-env op)
                 => (λ (v)
                      (match v
                        [(llvm-value value _)
                         (add-function-call-md! (LLVMBuildCall llvm-builder value
                                                               (map compile-value args)
                                                               (to-string result)) md)]
                        [else (error 'sham:llvm "cannot figure out how to apply value ~a of ~a" v op)]))]
                [(local-var-ref op)
                 => (λ (value)
                      (add-function-call-md! (LLVMBuildCall llvm-builder value
                                                            (map compile-value args)
                                                            (to-string result))
                                             md))]))]))
        (define (build-terminator-instruction! terminator)
          (match terminator
            [(llvm:ast:instruction:terminator:ret md value)
             (LLVMBuildRet llvm-builder (compile-value value))]
            [(llvm:ast:instruction:terminator:retv md)
             (LLVMBuildRetVoid llvm-builder)]
            [(llvm:ast:instruction:terminator:br md condition iftrue iffalse)
             (LLVMBuildCondBr llvm-builder (compile-value condition) (block-ref iftrue) (block-ref iffalse))]
            [(llvm:ast:instruction:terminator:bru md destinition)
             (LLVMBuildBr llvm-builder (block-ref destinition))]
            [(llvm:ast:instruction:terminator:switch md value default value-s dest-s)
             (define switch (LLVMBuildSwitch llvm-builder (compile-value value) (block-ref default) (length value-s)))
             (map (curry LLVMAddCase switch) (map compile-value value-s) (map block-ref dest-s))]))


        (match-define (llvm:ast:block md name instructions terminator) b)
        (define llvm-block (block-ref name))

        (LLVMPositionBuilderAtEnd llvm-builder llvm-block)
        (match instructions
          [(cons initial rst)
           (build-initial-instruction! initial)
           (map build-instruction! rst)]
          [empty (void)])
        (build-terminator-instruction! terminator))

      (map build-block! blocks)
      (add-function-def-info! function-ref (llvm:def-info def)))

    (define (compile-type-definition! def name type)
      (match type
        [(llvm:ast:type:struct _ fields)
         (define field-types (map (curryr compile-type internal-env decl-env) fields))
         (define type-ref (assoc-env-lookup decl-env name))
         (LLVMStructSetBody type-ref field-types #f)
         (add-type-def-info! type-ref (llvm:def-info def))]
        [else (error 'sham:llvm "trying to add type to declaration for non struct type")]))

    (match def
      [(llvm:def:type info type-name t)
       (compile-type-definition! def type-name t)
       (values type-name (assoc-env-lookup decl-env type-name))]
      [(llvm:def:function info function-name args types ret-type body)
       (compile-function-definition! def function-name args types ret-type body)
       (values function-name (assoc-env-lookup decl-env function-name))]
      [(llvm:def:global _ id _ _)
       (values id (assoc-env-lookup decl-env id))]))

  (match module-ast
    [(llvm:def:module info id defs)
     (define internal-env (create-internal-environment llvm-context))
     (define decl-env
       (for/fold ([decl-env (empty-assoc-env)])
                 ([def defs])
         (register-define def internal-env decl-env)))
     (llvm-env
      llvm-module
      llvm-context
      module-ast
      (for/hash ([def defs])
        (compile-define def internal-env decl-env)))]))
