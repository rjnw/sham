#lang racket

(require sham/llvm/ir
         sham/llvm/ir/simple
         sham/ir/ast/core
         sham/ir/env)

(provide diagnose-sham-builder
         build-llvm-ir)

(define diagnose-sham-builder (make-parameter #f))

(define ((add-to-list-box! b) v)
  (set-box! b (cons v (unbox b))))

(define (build-llvm-ir module-ast
                       [sham-context (global-sham-context)]
                       [llvm-target-triple #f]
                       [llvm-data-layout #f])
  (match-define (sham:def:module module-info module-name module-defs) module-ast)
  (define defs (box '()))
  (define externals (box '()))
  (define collect-def! (add-to-list-box! defs))
  (define collect-external! (add-to-list-box! externals))
  (define (translate-type t)
    (match t
      [(? llvm:ast:type?) t]
      [else (error 'sham:ir:builder "unknown type ast, expecting llvm type: ~a" t)]))
  (define (stmt->llvm-blocks body)
    (define incomplete-blocks (make-hash))
    (define finished-blocks (box '()))
    (define collect-block! (add-to-list-box! finished-blocks))
    (define (add-instruction! block-name instr)
      (unless (llvm:ast:instruction:op? instr)
        (error 'sham:ir:builder "expected op llvm instruction for adding to a block ~a/~a" block-name instr))
      (hash-set! incomplete-blocks block-name (cons instr (hash-ref incomplete-blocks block-name '()))))
    (define (terminate-block! block-name terminator)
      (unless (llvm:ast:instruction:terminator? terminator)
        (error 'sham:ir:builder "expected llvm terminator instruction for terminating block ~a/~a"
               block-name terminator))
      (define block-instrs (hash-ref incomplete-blocks block-name))
      (hash-remove! incomplete-blocks block-name)
      (collect-block! (ast-block block-name (reverse block-instrs) terminator))
      #f)
    ;; -> (values llvm-val [maybe current-block-name])
    (define (build-expr! expr current-block)
      (match expr
        [(sham:ast:expr:ref _ sym)]
        [(sham:ast:expr:op _ rator flags args)]
        [(sham:ast:expr:access _ struct-field value)]
        [(sham:ast:expr:void _)]
        [(sham:ast:expr:etype _)]
        [(sham:ast:expr:let _ ids vals types stmt expr)]))
    ;; -> [maybe current-block-name]
    (define (build-stmt! curr-stmt current-block continue-block break-block)
      (match curr-stmt
        [(sham:ast:set! _ lhs val)
         (match lhs
           [(sham:ast:expr:ref _ sym)
            (define-values (val-val val*) (build-expr! val current-block))
            (unless (block? val*) (error 'sham:ir:builder "set! value terminated block ~a" curr-stmt))
            (add-instruction! val* (store! #f val-val sym))
            val*])]
        [(sham:ast:if _ tst thn els)
         (let* ([entry-block (gensym 'if-entry)]
                [thn-block (gensym 'if-then)]
                [els-block (gensym 'if-else)]
                [after-block (gensym 'if-after)])
           (define-values (tst-val tst*) (build-expr! tst entry-block))
           (unless (block? tst*) (error 'sham:ir:builder "test value for if terminated block! ~a" curr-stmt))
           (terminate-block! tst* (ast-br tst-val then-block else-block))
           (define thn* (build-stmt! thn thn-block entry-block after-block))
           (unless (block? thn*) (terminate-block! thn* (ast-bru after-block)))
           (define els* (build-stmt! els els-block entry-block after-block))
           (unless (block? els*) (terminate-block! els* (ast-bru after-block)))
           (terminate-block! current-block (ast-bru entry-block))
           (if (or (block? thn*) (block? els*)) after-block #f))]
        [(sham:ast:switch _ tst chcks thns deflt)
         (let* ([entry-block (gensym 'switch-entry)]
                [case-blocks (build-list (length thns) (λ (i) (gensym (string->symbol (format "switch-~a" i)))))]
                [dflt-block (gensym 'switch-default)]
                [after-block (gensym 'switch-after)])
           (define-values (tst-val tst*) (build-expr! tst entry-block))
           (unless (block? tst*) (error 'sham:ir:builder "test value for switch terminated block! ~a" curr-stmt))
           (define check-values
             (for/list ([c chcks])
               (define-values (c-val c*) (build-expr! c entry-block))
               (unless (eq? c* entry-block)
                 (error 'sham:ir:builder "switch check value caused block generation ~a/~a" curr-stmt c))
               c-val))
           (terminate-block! tst* (ast-switch tst-val default-block check-values case-blocks))
           (define fin?
             (for/fold ([fin? #t])
                       ([t thns]
                        [b case-blocks]
                        [n (append (cdr case-blocks) (list after-block))])
               (define t* (build-stmt! t b continue-block after-block))
               (when (block? t*) (terminate-block! t* (ast-bru n)))
               (and (not (block? t*)) fin?)))
           (define dflt* (build-stmt! deflt dflt-block continue-block after-block))
           (when (block? dflt*) (terminate-block! dflt* (ast-bru after-block)))
           (if (or (not fin?) (block? dflt*)) after-block #f))]
        [(sham:ast:continue _)
         (terminate-block! current-block (ast-bru continue-block))]
        [(sham:ast:break _)
         (terminate-block! current-block (ast-bru break-block))]
        [(sham:ast:while _ tst body)]
        [(sham:ast:void _) current-block]
        [(sham:ast:expr _ e)
         (define-values (val block*) (build-expr! e current-block))
         block*]
        [(sham:ast:block _ stmts)
         (for/fold ([block current-block])
                   ([s stmts])
           (unless (block? block) (error 'sham:ir:builder "stmt terminated in sham:ast:block ~a" curr-stmt))
           (build-stmt! s block continue-block break-block))]
        [(sham:ast:label _ label-block body)
         (terminate-block! current-block (ast-bru label-block))
         (build-stmt! body label-block continue-block break-block)]
        [(sham:ast:return _ val)
         (define-values (val-val block*) (build-expr! val current-block))
         (unless (block? block*) (error 'sham:ir:builder "return value itself terminated block ~a" curr-stmt))
         (terminate-block! block* (ast-ret val-val))]
        [(sham:ast:return-void _)
         (terminate-block! current-block (ast-retv))]))
    (reverse (unbox finished-blocks)))
  (define (collect-sham-function! def)
    (match-define (sham:def:function info name type body) def)
    (collect-def! (llvm:def:function info name
                                     (translate-type type)
                                     (stmt->llvm-blocks body))))
  (define (collect-sham-struct! def)
    (match-define (sham:def:struct info name fields types) def)
    (define llvm-struct (llvm:ast:type:struct (map translate-type types)))
    (collect-def! (llvm:def:type info name llvm-struct)))
  (define (collect-sham-racket! def)
    (match-define (sham:def:racket info name value type) def)
    (define rkt-def (llvm:def:external info name (translate-type type)))
    (collect-def! rkt-def)
    (collect-jit-rkt-external! def))
  (define (translate&collect-def def)
    (match def
      [(? llvm:def?) (collect-def! def)]
      [(? sham:def:function?) (collect-sham-function! def)]
      [(? sham:def:struct?) (collect-sham-struct! def)]
      [(? sham:def:racket?) (collect-sham-racket! def)]))

  (map translate&collect-def! module-defs)
  (sham-env module-ast
            (llvm:def:module module-info module-name (reverse (unbox defs)))
            (unbox externals)))
