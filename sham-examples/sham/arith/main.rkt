#lang racket

(require sham/sam/ast
         sham/sam/rkt
         sham/sam/transform
         sham/ir
         sham/jit
         (prefix-in ll- sham/llvm/ir/simple)
         syntax/parse/define
         syntax/parse)

(define-ast arir
  (expr
   [add (es:expr ...)]
   [sub (es:expr ...)]
   [mul (es:expr ...)]
   [div (es:expr ...)]
   [var (n:int)]
   [val (n:int)])
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
   [(val v) (ui64 v)]
   [(var s) (expr-ref s)]))

(define-transform (all-vars)
  (arir -> rkt-value)
  (cexpr (expr -> any)
   [(add (^ es) ...) (apply set-union es)]
   [(sub (^ es) ...) (apply set-union es)]
   [(mul (^ es) ...) (apply set-union es)]
   [(div (^ es) ...) (apply set-union es)]
   [(val v) (set)]
   [(var s) (set s)]))

(define-transform (stx-to-arith)
  (rkt-syntax -> arir)
  (ce (stx -> expr)
      [('+ e:ce ...) (make add e)]
      [('- e:ce ...) (make sub e)]
      [('* e:ce ...) (make mul e)]
      [('/ e:ce ...) (make div e)]
      [n:integer (make val (syntax-e n))]
      [('var n:integer) (make var (syntax->datum n))]
      [n:id (make var (string->number (format "~a" (syntax-e n))))]))

(define (fold-constants op v0 vs)
  (for/fold ([res v0]
             [rst '()]
             #:result (cons (expr-val res) (reverse rst)))
            ([v vs])
    (if (expr-val? v)
        (values (op (expr-val-n v) res) rst)
        (values res (cons v rst)))))

(define-transform (constant-fold)
  (arir -> arir)
  (ce (expr -> expr)
   [(add (^ es) ...) (make add (fold-constants + 0 es))]
   [(sub (val vs) ...) (make val (apply - vs))]
   [(mul (^ es) ...) (make add (fold-constants * 1 es))]
   [(div (val vs) ...) (make val (apply / vs))]
   [e e]))

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

(define-syntax (arith stx)
  (syntax-case stx ()
    [(_ s) #`(compile-arith (stx-to-arith #'s))]))

(module+ test
  (require rackunit)
  (define fid (compile-arith (expr-var 0)))
  (check-equal? (fid 42) 42)

  (define fadd (compile-arith (expr-add (expr-var 0) (expr-var 1))))
  (check-equal? (fadd 21 21) 42)

  (check-equal? ((arith (+ \0 \1)) 21 21) 42))
