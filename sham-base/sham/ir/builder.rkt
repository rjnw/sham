#lang racket

(require syntax/parse/define)

(require sham/llvm/ir
         sham/sam/runtime
         sham/md
         sham/llvm/ir/simple
         (prefix-in op- (submod sham/llvm/ir/specific ops))
         sham/llvm/ir/env
         sham/ir/ast
         sham/ir/env
         sham/ir/dump
         sham/rkt/ffi
         sham/rkt/conv
         sham/parameters)

(require sham/private/box)

(provide debug-sham-builder
         build-sham-env
         build-sham-module)

(define debug-sham-builder (make-parameter #f))

(define (build-sham-env module-ast
                        [sham-context (sham-global-context)]
                        [llvm-target-triple #f]
                        [llvm-data-layout #f]
                        #:debug (debug-flag #f))
  (define s-mod (build-sham-module module-ast sham-context llvm-target-triple llvm-data-layout))
  (match-define (sham-module ast l-ast extrs) s-mod)
  (when (or debug-flag (debug-sham-builder)) (sham-dump-llvm s-mod))
  (sham-env s-mod (build-llvm-env l-ast) (make-hash)))

(define (build-sham-module module-ast
                           [sham-context (sham-global-context)]
                           [llvm-target-triple #f]
                           [llvm-data-layout #f])
  (match-define (sham:def:module #:md module-md module-name module-defs ...) module-ast)
  (define def-map (for/hash ([def module-defs]) (values (sham:def-id def) def)))
  (define (lookup-def name) (hash-ref def-map name))
  (define defs (box '()))
  (define collect-def! (add-to-list-box! defs))
  (define externals (box '()))
  (define collect-external! (add-to-list-box! externals))
  (define intrinsic-map (make-hash))
  (define external-map (make-hash))
  (define (collect-jit-rkt-external! llvm-def sham-def)
    (match-define (sham:def:racket name value type rkt-type) sham-def)
    (collect-external! (external-mapping name (func-uintptr value rkt-type))))
  (define (collect-jit-lib-external! llvm-def sham-rator)
    (match-define (sham:rator:external lib-id id type var-arg?) sham-rator)
    (match-define (llvm:def:external name llvm-type) llvm-def)
    (collect-external! (external-mapping name (get-lib-uintptr lib-id id))))
  (define (translate-type t)
    (match t
      [(? llvm:type?) t]
      [else (error 'sham:ir:builder "unknown type ast, expecting llvm type: ~a" t)]))
  (define (stmt->llvm-blocks body)
    (define incomplete-blocks (make-hash))
    (define finished-blocks (box '()))
    (define collect-block! (add-to-list-box! finished-blocks))
    (define block? symbol?)
    (define (add-instruction! block-name instr)
      (unless (llvm:instruction:op? instr)
        (error 'sham:ir:builder "expected op llvm instruction for adding to a block ~a/~a" block-name instr))
      (unless (block? block-name)
        (error 'sham:ir:builder "trying to add instruction to terminated block ~a" instr))
      (hash-set! incomplete-blocks block-name (cons instr (hash-ref incomplete-blocks block-name '())))
      block-name)
    (define (terminate-block! block-name terminator)
      (unless (llvm:instruction:terminator? terminator)
        (error 'sham:ir:builder "expected llvm terminator instruction for terminating block ~a/~a"
               block-name terminator))
      (unless (block? block-name)
        (error 'sham:ir:builder "trying to terminate already terminated block ~a" terminator))
      (define block-instrs (hash-ref incomplete-blocks block-name '()))
      (hash-remove! incomplete-blocks block-name)
      (collect-block! (make-def-block block-name (reverse block-instrs) terminator))
      #f)
    (define-simple-macro (check-terminate! block-name terminator)
      (when (block? block-name)
        (terminate-block! block-name terminator)))
    (define allocas (box '()))
    (define add-alloca! (add-to-list-box! allocas))
    (define (add-allocas! names types)
      (for ([n names]
            [t types])
        (add-alloca! (op-alloca n ((translate-type t))))))
    (define entry-block 'entry-block)
    (define (alloca-block)
      (make-def-block 'alloca-block (reverse (unbox allocas)) (inst-bru entry-block)))

    ;; -> (values llvm-val? [maybe current-block-name])
    (define (build-expr! expr current-block continue-block break-block)
      (define (errored-terminate block-name value type)
        (unless (block? block-name)
          (error 'sham:ir:builder "~a value for ~a terminated block while building expr ~a" value type expr)))

      (match expr
        [(? llvm:value?) (values expr current-block)]
        [(? llvm:instruction:op?)
         (add-instruction! current-block expr)
         (values (inst-op-result expr) current-block)]
        [(sham:expr:ref (? number? n))
         (values (val-param n) current-block)]
        [(sham:expr:ref sym)
         (define expr-value (gensym 'base-ref))
         (add-instruction! current-block (op-load expr-value (sym)))
         (values expr-value current-block)]
        [(sham:expr:op #:md md rator args ...)
         (define expr-value (gensym 'op))
         (define arg-vals
           (for/list ([arg args])
             (define-values (arg-val arg*) (build-expr! arg current-block continue-block break-block))
             (unless (equal? arg* current-block)
               (error 'sham:ir:builder "expr op values changed a block ~a/~a" arg expr))
             arg-val))
         (define-values (llvm-rator new-block)
           (match rator
             [(? (or/c symbol? string?)) (values rator current-block)]
             [(? sham:expr?)
              (build-expr! rator current-block continue-block break-block)]
             [(sham:rator:reference s) (values s current-block)]
             [(sham:rator:intrinsic #:md md id type)
              (values (hash-ref! intrinsic-map rator
                                 (thunk
                                  (let ([intrinsic-name (gensym (if (string? id) (string->symbol id) id))])
                                    (collect-def! (llvm:def:intrinsic #:md md intrinsic-name id type))
                                    intrinsic-name)))
                      current-block)]
             [(sham:rator:external #:md md lib-id id type var-arg?)
              (values (hash-ref! external-map rator
                                 (thunk
                                  (define external-name (gensym (string->symbol (format "~a-~a" id (or lib-id "")))))
                                  (define llvm-def (llvm:def:external #:md md external-name (translate-type type)))
                                  (collect-def! llvm-def)
                                  (collect-jit-lib-external! llvm-def rator)
                                  external-name))
                      current-block)]
             [else (error 'sham:ir:builder "unknown rator ~a" rator)]))
         (add-instruction! new-block (make-inst-op expr-value llvm-rator '() arg-vals))
         (values expr-value new-block)]
        [(sham:expr:access struct-field value)
         (match-define (cons struct-name field-name) struct-field)
         (match-define (sham:def:struct name (fields typs) ...) (lookup-def struct-name))
         (define field-index (index-of fields field-name))
         (define expr-value (gensym 'struct-field))
         (define field-ptr (gensym 'struct-field-ptr))
         (define-values (val block*) (build-expr! value current-block continue-block break-block))
         (errored-terminate block* "expr" "struct access")
         (add-instruction! block* (op-gep field-ptr [val (val-ui 0 i32) (val-ui field-index i32)]))
         (add-instruction! block* (op-load expr-value [(field-ptr)]))
         (values expr-value block*)]
        [(sham:expr:void) (values #f current-block)]
        [(sham:expr:etype t) (values (translate-type t) current-block)]
        [(sham:expr:let [(id-names vals types) ...] stmt-body expr-body)
         (define let-val-block (gensym 'let-values))
         (add-allocas! id-names types)
         (terminate-block! current-block (inst-bru let-val-block))
         (define block*
           (for/fold ([block* let-val-block])
                     ([v vals]
                      [i id-names]
                      #:when v)
             (define-values (v-val v*) (build-expr! v block* continue-block break-block))
             (add-instruction! v* (op-store! #f (v-val i)))
             v*))
         (errored-terminate block* "one of the binding" "let")
         (define stmt* (build-stmt! stmt-body block* continue-block break-block))
         (build-expr! expr-body stmt* continue-block break-block)]))
    ;; -> [maybe current-block-name]
    (define (build-stmt! curr-stmt current-block continue-block break-block)
      (define (errored-terminate block-name value type)
        (unless (block? block-name)
          (error 'sham:ir:builder "~a value for ~a terminated block while building stmt ~a" value type curr-stmt)))
      (define-simple-macro (warn str args ...)
        (when (debug-sham-builder)
          (eprintf "sham:warning: ~a" (format str args ...))))
      (define (if-block block-name next-block)
        (if (block? block-name) next-block #f))
      (match curr-stmt
        [(? llvm:instruction:terminator?)
         (terminate-block! current-block curr-stmt)]
        [(? llvm:instruction?)
         (add-instruction! current-block curr-stmt)]
        [(sham:stmt:set! lhs val)
         (match lhs
           [(sham:expr:ref sym)
            (define-values (val-val val*) (build-expr! val current-block continue-block break-block))
            (errored-terminate val* "lhs" "set!")
            (add-instruction! val* (op-store! #f (val-val sym)))
            val*])]
        [(sham:stmt:if tst thn els)
         (let* ([entry-block (gensym 'if-entry)]
                [then-block (gensym 'if-then)]
                [else-block (gensym 'if-else)]
                [after-block (gensym 'if-after)])
           (terminate-block! current-block (inst-bru entry-block))
           (define-values (tst-val tst*) (build-expr! tst entry-block continue-block break-block))
           (errored-terminate tst* "test" "if")
           (terminate-block! tst* (inst-br tst-val then-block else-block))
           (define thn* (build-stmt! thn then-block entry-block after-block))
           (check-terminate! thn* (inst-bru after-block))
           (define els* (build-stmt! els else-block entry-block after-block))
           (check-terminate! els* (inst-bru after-block))
           (if (or (block? thn*) (block? els*)) after-block #f))]
        [(sham:stmt:switch tst (chcks thns) ... dflt)
         (let* ([entry-block (gensym 'switch-entry)]
                [case-blocks (build-list (length thns) (λ (i) (gensym (string->symbol (format "switch-~a" i)))))]
                [dflt-block (gensym 'switch-default)]
                [after-block (gensym 'switch-after)])
           (terminate-block! current-block (inst-bru entry-block))
           (define-values (tst-val tst*) (build-expr! tst entry-block continue-block break-block))
           (errored-terminate tst* "test" "switch")
           (define check-values
             (for/list ([c chcks])
               (define-values (c-val c*) (build-expr! c entry-block continue-block after-block))
               (unless (eq? c* entry-block)
                 (error 'sham:ir:builder "switch check value caused block generation ~a/~a" curr-stmt c))
               c-val))
           (terminate-block! tst* (make-inst-switch tst-val dflt-block check-values case-blocks))
           (define fin?
             (if (null? case-blocks)
                 #t
                 (for/fold ([fin? #t])
                           ([t thns]
                            [b case-blocks]
                            [n (append (cdr case-blocks) (list dflt-block))])
                   (define t* (build-stmt! t b continue-block after-block))
                   (check-terminate! t* (inst-bru n))
                   (and (not (block? t*)) fin?))))
           (define dflt* (build-stmt! dflt dflt-block continue-block after-block))
           (check-terminate! dflt* (inst-bru after-block))
           (if (or (not fin?) (block? dflt*)) after-block #f))]
        [(sham:stmt:continue)
         (terminate-block! current-block (inst-bru continue-block))]
        [(sham:stmt:break)
         (terminate-block! current-block (inst-bru break-block))]
        [(sham:stmt:while tst body)
         (let* ([entry-block (gensym 'loop-entry)]
                [loop-block (gensym 'loop-body)]
                [after-block (gensym 'loop-after)])
           (terminate-block! current-block (inst-bru entry-block))
           (define-values (tst-val tst*) (build-expr! tst entry-block continue-block after-block))
           (errored-terminate tst* "test" "while")
           (terminate-block! tst* (inst-br tst-val loop-block after-block))
           (define body* (build-stmt! body loop-block entry-block after-block))
           (if (block? body*)
               (terminate-block! body* (inst-bru entry-block))
               (warn "while body terminated! ~a" curr-stmt))
           (if-block body* after-block))]
        [(sham:stmt:void) current-block]
        [(sham:stmt:expr e)
         (define-values (val block*) (build-expr! e current-block continue-block break-block))
         block*]
        [(sham:stmt:block stmts ...)
         (for/fold ([block current-block])
                   ([s stmts])
           (build-stmt! s block continue-block break-block))]
        ;; [(sham:stmt:label label-block body)
        ;;  (check-terminate! current-block (inst-bru label-block))
        ;;  (build-stmt! body label-block continue-block break-block)]
        [(sham:stmt:return val)
         (define-values (val-val block*) (build-expr! val current-block continue-block break-block))
         (errored-terminate block* "expr" "return")
         (terminate-block! block* (inst-ret val-val))]
        [(sham:stmt:return-void)
         (terminate-block! current-block (inst-retv))]
        [s (error 'sham "invalid sham statement: ~a" s)]))
    (match body
      [(list llvm-blocks ... base-stmt)
       (map collect-block! llvm-blocks)
       (build-stmt! body entry-block #f #f)]
      [else (build-stmt! body entry-block #f #f)])
    (cons (alloca-block) (reverse (unbox finished-blocks))))

  (define (collect-sham-function! def)
    (match-define (sham:def:function #:md md name type body) def)
    (collect-def! (make-llvm:def:function #:md md name
                                     (translate-type type)
                                     (stmt->llvm-blocks body))))
  (define (collect-sham-struct! def)
    (match-define (sham:def:struct #:md md name (fields types) ...) def)
    (define llvm-struct (make-llvm:type:struct (map translate-type types)))
    (collect-def! (llvm:def:type #:md md name llvm-struct)))
  (define (collect-sham-racket! def)
    (match-define (sham:def:racket #:md md name value type _) def)
    (define llvm-def (llvm:def:external #:md md name (translate-type type)))
    (collect-def! llvm-def)
    (collect-jit-rkt-external! llvm-def def))
  (define (translate&collect-def! def)
    (match def
      [(? llvm:def?) (collect-def! def)]
      [(? sham:def:function?) (collect-sham-function! def)]
      [(? sham:def:struct?) (collect-sham-struct! def)]
      [(? sham:def:racket?) (collect-sham-racket! def)]))

  (map translate&collect-def! module-defs)
  (define mmd (llvm-metadata module-md))
  (define new-module-md
    (ast:metadata
     (ast:metadata-locs module-md)
     (set-module-md-jit-external-mappings! mmd (append (or (ref-module-md-jit-external-mappings mmd) '()) (unbox externals)))))
  (sham-module module-ast
               (make-llvm:def:module #:md new-module-md module-name (reverse (unbox defs)))
               (unbox externals)))
