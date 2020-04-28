#lang racket

(require sham/llvm/ir/ast
         sham/llvm/ir/env
         sham/llvm/ir/context
         sham/llvm/ir/internals
         sham/llvm/ir/md
         sham/llvm/ffi
         ffi/unsafe)

(provide diagnose-llvm-builder
         build-llvm-module)

(define diagnose-llvm-builder (make-parameter #f))

(define (build-llvm-module module-ast
                           [llvm-context (global-llvm-context)]
                           #:target-triple [llvm-target-triple (LLVMGetDefaultTargetTriple)]
                           #:data-layout [llvm-data-layout #f])
  (when (diagnose-llvm-builder)
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
      [(llvm:def:function info name type body)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddFunction llvm-module (to-string name) type-ref))
       (assoc-env-extend decl-env name (llvm-function value-ref type-ref))]
      [(llvm:def:global info name type value)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddGlobal llvm-module type-ref (to-string name)))
       (LLVMSetInitializer value-ref
                           (if value (compile-constant value internal-env decl-env) (LLVMConstNull type-ref)))
       (assoc-env-extend decl-env name (llvm-value value-ref type-ref))]
      [(llvm:def:external info name type)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddGlobal llvm-module
                                        type-ref
                                        (to-string name)))
       (assoc-env-extend decl-env name (llvm-external value-ref type-ref))]
      [(llvm:def:intrinsic info id name type)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddFunction llvm-module
                                          (to-string name)
                                          type-ref))
       (assoc-env-extend decl-env id (llvm-intrinsic value-ref type-ref))]))

  ;; llvm:ast:type env -> LLVMTypeRef
  (define (compile-type type internal-env decl-env)
    (match type
      [(llvm:ast:type:ref _ t)
       (if (assoc-env-contains? decl-env t)
           (assoc-env-lookup decl-env t)
           (assoc-env-lookup internal-env t))]
      [(llvm:ast:type:struct _ fields)
       (LLVMStructType (map (curryr compile-type internal-env decl-env) fields) #f)]
      [(llvm:ast:type:function _ args var-arg? ret)
       (LLVMFunctionType (compile-type ret internal-env decl-env)
                         (map (curryr compile-type internal-env decl-env) args)
                         var-arg?)]
      [(llvm:ast:type:pointer _ to)
       (LLVMPointerType (compile-type to internal-env decl-env) 0)]
      [(llvm:ast:type:array _ of size)
       (LLVMArrayType (compile-type of internal-env decl-env) size)]
      [(llvm:ast:type:vector _ of size)
       (LLVMVectorType (compile-type of internal-env decl-env) size)]))

  (define (compile-constant v internal-env decl-env (value-compiler compile-constant))
    (match v
      [(llvm:ast:value:fl md value t)
       (LLVMConstReal (compile-type t internal-env decl-env) value)]
      [(llvm:ast:value:si md value t)
       (LLVMConstInt (compile-type t internal-env decl-env) (cast value _sint64 _uint64) #f)]
      [(llvm:ast:value:ui md value t)
       (LLVMConstInt (compile-type t internal-env decl-env) value #f)]
      [(llvm:ast:value:llvm md v) v]
      [(llvm:ast:value:basic-struct md fields)
       (LLVMConstStruct (map (curryr value-compiler internal-env decl-env) fields) #f)]
      [(llvm:ast:value:named-struct md fields type)
       (LLVMConstNamedStruct (compile-type type internal-env decl-env)
                             (map (curryr value-compiler internal-env decl-env) fields))]
      [(llvm:ast:value:array md vals t)
       (LLVMConstArray (compile-type t internal-env decl-env)
                       (map (curryr value-compiler internal-env decl-env) vals))]
      [(llvm:ast:value:string md str)
       (LLVMBuildGlobalStringPtr llvm-builder str (to-string (gensym 'str)))]
      [(llvm:ast:value:vector md vals)
       (LLVMConstVector (map (curryr value-compiler internal-env decl-env) vals))]
      [(llvm:ast:value:sizeof md type)
       (LLVMSizeOf (compile-type type internal-env decl-env))]))

  (define (compile-define def internal-env decl-env)
    (define (compile-function-definition! def name type blocks)
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
            [(? exact-nonnegative-integer?) (LLVMGetParam function-ref v)]
            [(llvm:ast:value:ref md sym)
             (if (assoc-env-contains? decl-env sym) (assoc-env-lookup decl-env sym) (local-var-ref sym))]
            [(llvm:ast:value:param md i) (LLVMGetParam function-ref i)]
            [(? llvm:ast:value?) (compile-constant v internal-env decl-env
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
          (match instruction
            [(llvm:ast:instruction:op md result op flags args)
             (add-local-var!
              result
              (add-instruction-md!
               (cond
                 [(assoc-env-ref internal-env op)
                  => (λ (opbuilder) (opbuilder llvm-builder (cons md flags) args
                                               (curryr compile-type internal-env decl-env)
                                               compile-value
                                               (to-string result)))]
                 [(assoc-env-ref decl-env op)
                  => (λ (v)
                       (match v
                         [(llvm-value value _)
                          (LLVMBuildCall llvm-builder value
                                         (map compile-value args)
                                         (to-string result))]
                         [else (error 'sham:llvm "cannot figure out how to apply value ~a of ~a" v op)]))]
                 [(local-var-ref op)
                  => (λ (value)
                       (LLVMBuildCall llvm-builder value
                                      (map compile-value args)
                                      (to-string result)))])
               md flags))]))
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
      [(llvm:def:function info function-name type body)
       (compile-function-definition! def function-name type body)
       (values function-name (assoc-env-lookup decl-env function-name))]
      [(llvm:def _ id)
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
