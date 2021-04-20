#lang racket

(require sham/llvm/ir/ast
         sham/llvm/ir/env
         sham/private/env
         sham/llvm/ir/context
         sham/llvm/ir/internals
         sham/llvm/ir/md
         sham/llvm/ffi
         ffi/unsafe)

(provide diagnose-llvm-builder
         build-llvm-env)

(define diagnose-llvm-builder (make-parameter #f))

(define (build-llvm-env module-ast
                        [llvm-context (global-llvm-context)]
                        #:target-triple [llvm-target-triple (LLVMGetDefaultTargetTriple)]
                        #:data-layout [llvm-data-layout #f])
  (when (diagnose-llvm-builder)
    (define (diag-handler dmd voidp)
      (define diag-desc (LLVMGetDiagInfoDescription dmd))
      (eprintf "llvm-diagnose: ~a\n" (cast diag-desc _pointer _string))
      (LLVMDisposeMessage diag-desc))
    (LLVMContextSetDiagnosticHandler llvm-context diag-handler #f))
  (match-define (llvm:def:module #:md module-md module-name module-defs) module-ast)
  (define llvm-module (LLVMModuleCreateWithNameInContext (to-string module-name) llvm-context))
  (define llvm-builder (LLVMCreateBuilderInContext llvm-context))
  (when llvm-data-layout (LLVMSetDataLayout llvm-module llvm-data-layout))
  (LLVMSetTarget llvm-module llvm-target-triple)

  ;; llvm:def LLVMModuleRef env -> env
  (define (register-define def internal-env decl-env)
    (match def
      [(llvm:def:type name t)
       (if (llvm:type:struct? t)
           (assoc-env-extend decl-env name (LLVMStructCreateNamed llvm-context (to-string name)))
           decl-env)]
      [(llvm:def:function name type body)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddFunction llvm-module (to-string name) type-ref))
       (assoc-env-extend decl-env name (llvm-function value-ref type-ref))]
      [(llvm:def:global name type value)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddGlobal llvm-module type-ref (to-string name)))
       (LLVMSetInitializer value-ref
                           (if value (compile-constant value internal-env decl-env) (LLVMConstNull type-ref)))
       (assoc-env-extend decl-env name (llvm-value value-ref type-ref))]
      [(llvm:def:external name type)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddGlobal llvm-module
                                        type-ref
                                        (to-string name)))
       (assoc-env-extend decl-env name (llvm-external value-ref type-ref))]
      [(llvm:def:intrinsic id name type)
       (define type-ref (compile-type type internal-env decl-env))
       (define value-ref (LLVMAddFunction llvm-module
                                          (to-string name)
                                          type-ref))
       (assoc-env-extend decl-env id (llvm-intrinsic value-ref type-ref))]))

  ;; llvm:type env -> LLVMTypeRef
  (define (compile-type type internal-env decl-env)
    (match type
      [(llvm:type:ref t)
       (if (assoc-env-contains? decl-env t)
           (assoc-env-lookup decl-env t)
           (assoc-env-lookup internal-env t))]
      [(llvm:type:struct fields ...)
       (LLVMStructType (map (curryr compile-type internal-env decl-env) fields) #f)]
      [(llvm:type:function args ... var-arg? ret)
       (LLVMFunctionType (compile-type ret internal-env decl-env)
                         (map (curryr compile-type internal-env decl-env) args)
                         var-arg?)]
      [(llvm:type:pointer to)
       (LLVMPointerType (compile-type to internal-env decl-env) 0)]
      [(llvm:type:array of size)
       (LLVMArrayType (compile-type of internal-env decl-env) size)]
      [(llvm:type:vector of size)
       (LLVMVectorType (compile-type of internal-env decl-env) size)]))

  (define (compile-constant v internal-env decl-env (value-compiler compile-constant))
    (match v
      [(llvm:value:fl value t)
       (LLVMConstReal (compile-type t internal-env decl-env) value)]
      [(llvm:value:si value t)
       (LLVMConstInt (compile-type t internal-env decl-env) (cast value _sint64 _uint64) #f)]
      [(llvm:value:ui value t)
       (LLVMConstInt (compile-type t internal-env decl-env) value #f)]
      [(llvm:value:llvm v) v]
      [(llvm:value:basic-struct fields)
       (LLVMConstStruct (map (curryr value-compiler internal-env decl-env) fields) #f)]
      [(llvm:value:named-struct fields type)
       (LLVMConstNamedStruct (compile-type type internal-env decl-env)
                             (map (curryr value-compiler internal-env decl-env) fields))]
      [(llvm:value:array vals t)
       (LLVMConstArray (compile-type t internal-env decl-env)
                       (map (curryr value-compiler internal-env decl-env) vals))]
      [(llvm:value:string str)
       (LLVMBuildGlobalStringPtr llvm-builder str (to-string (gensym 'str)))]
      [(llvm:value:vector vals ...)
       (LLVMConstVector (map (curryr value-compiler internal-env decl-env) vals))]
      [(llvm:value:sizeof type)
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
          [(llvm:def:block name instructions ... terminator)
           (hash-set! block-map name (append-block! name))]
          [else (error 'sham:llvm "invalid block in function definitions ~a" b)]))
      (define (build-block! b)
        (define (compile-value v)
          (match v
            [(? (curry assoc-env-contains? decl-env)) (assoc-env-lookup decl-env v)]
            [(? symbol?) (local-var-ref v)]
            [(? exact-nonnegative-integer?) (LLVMGetParam function-ref v)]
            [(llvm:value:ref sym)
             (if (assoc-env-contains? decl-env sym) (assoc-env-lookup decl-env sym) (local-var-ref sym))]
            [(llvm:value:param i) (LLVMGetParam function-ref i)]
            [(? llvm:value?) (compile-constant v internal-env decl-env
                                                   (λ (v ie de) (compile-value v)))]
            [(? llvm:type?) (compile-type v internal-env decl-env)]
            [else (error 'sham:llvm "unknown value ~a" v)]))
        (define (build-initial-instruction! instruction)
          (match instruction
            [(llvm:instruction:op #:md md result 'phi flags type vars labels)
             (define value (LLVMBuildPhi llvm-builder (compile-type type internal-env decl-env) (to-string result)))
             (LLVMAddIncoming value (map compile-value vars) (map block-ref labels))
             value]
            [else (build-instruction! instruction)]))
        (define (build-instruction! instruction)
          (match instruction
            [(llvm:instruction:op #:md md result op flags args ...)
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
            [(llvm:instruction:terminator:ret value)
             (LLVMBuildRet llvm-builder (compile-value value))]
            [(llvm:instruction:terminator:retv)
             (LLVMBuildRetVoid llvm-builder)]
            [(llvm:instruction:terminator:br condition iftrue iffalse)
             (LLVMBuildCondBr llvm-builder (compile-value condition) (block-ref iftrue) (block-ref iffalse))]
            [(llvm:instruction:terminator:bru destinition)
             (LLVMBuildBr llvm-builder (block-ref destinition))]
            [(llvm:instruction:terminator:switch value default (value-s dest-s) ...)
             (define switch (LLVMBuildSwitch llvm-builder (compile-value value) (block-ref default) (length value-s)))
             (map (curry LLVMAddCase switch) (map compile-value value-s) (map block-ref dest-s))]))


        (match-define (llvm:def:block name instructions ... terminator) b)
        (define llvm-block (block-ref name))

        (LLVMPositionBuilderAtEnd llvm-builder llvm-block)
        (match instructions
          [(cons initial rst)
           (build-initial-instruction! initial)
           (map build-instruction! rst)]
          [empty (void)])
        (build-terminator-instruction! terminator))

      (map build-block! blocks)
      (add-function-def-md! function-ref (llvm-metadata def)))

    (define (compile-type-definition! def name type)
      (match type
        [(llvm:type:struct fields ...)
         (define field-types (map (curryr compile-type internal-env decl-env) fields))
         (define type-ref (assoc-env-lookup decl-env name))
         (LLVMStructSetBody type-ref field-types #f)
         (add-type-def-md! type-ref (llvm-metadata def))]
        [else (error 'sham:llvm "trying to add type to declaration for non struct type")]))

    (match def
      [(llvm:def:type type-name t)
       (compile-type-definition! def type-name t)
       (values type-name (assoc-env-lookup decl-env type-name))]
      [(llvm:def:function function-name type body)
       (compile-function-definition! def function-name type body)
       (values function-name (assoc-env-lookup decl-env function-name))]
      [(llvm:def id)
       (values id (assoc-env-lookup decl-env id))]))

  (match module-ast
    [(llvm:def:module id defs ...)
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
