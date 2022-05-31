#lang racket

(require sham/sam/ast
         sham/sam/rkt
         sham/sam/transform
         sham/ir
         sham/jit
         (prefix-in ll- sham/llvm/ir/simple)
         syntax/parse/define)

(define-ast arir
  (expr
   [add (es:expr ...)]
   [sub (es:expr ...)]
   [mul (es:expr ...)]
   [div (es:expr ...)]
   [var (n:int)])
  #:format group-node)

(define-syntax-parse-rule (fold-op opf v0 vs)
  (for/fold ([res v0])
            ([v vs])
    (opf v res)))

(define-transform (arith-to-sham)
  (arir -> sham)
  (expr (expr -> expr)
   [(add (^ es) ...) (fold-op op-add (ui64 0) es)]
   [(sub (^ e)) (op-neg e)]
   [(sub (^ e1) (^ es) ...) (fold-op op-sub e1 es)]
   [(mul (^ es) ...) (fold-op op-mul (ui64 1) es)]
   [(div (^ e)) (op-udiv (ui64 1) e)]
   [(div (^ e1) (^ es) ...) (fold-op op-udiv e1 es)]
   [(var s) (expr-ref s)]))

(define-transform (all-vars)
  (arir -> rkt-value)
  (cexpr (expr -> any)
   [(add (^ es) ...) (apply set-union es)]
   [(sub (^ es) ...) (apply set-union es)]
   [(mul (^ es) ...) (apply set-union es)]
   [(div (^ es) ...) (apply set-union es)]
   [(var s) (set s)]))

(define (arith-sham-function name arir)
  (define nvars (add1 (apply max (set->list (all-vars arir)))))
  (make-def-function
   name (ll-make-type-function (build-list nvars (const i64)) #f i64)
   (stmt-return (arith-to-sham arir))))

(define (compile-arith arir)
  (define fname (gensym 'math))
  (define tmod (def-module (gensym 'arith-module) (arith-sham-function fname arir)))
  (define s-env (build-sham-env tmod))
  (define mc-env (initialize-jit s-env))
  (define func (jit-lookup-function mc-env fname))
  func)

(module+ test
  (require rackunit)
  (define fid (compile-arith (expr-var 0)))
  (check-equal? (fid 42) 42)

  (define fadd (compile-arith (expr-add (expr-var 0) (expr-var 1))))
  (check-equal? (fadd 21 21) 42))
