#lang racket

(require sham/sam/runtime/identifier
         sham/sam/runtime/ast
         racket/syntax)
(require (for-template (prefix-in ll- sham/llvm/ir)
                       sham/ir))
(require "ast.rkt")
(provide to-sham)

(define cfuns (box '()))
(define (add-cfun c) (printf "adding cfun: ~a\n" c) (set-box! cfuns (cons c (unbox cfuns))))
(define special-funs (make-hash))
(define special-names (make-hash))
(define gensym-map (make-hash))
(define (new-name old)
  (define (new-id! dat)
    (define cnt (hash-ref gensym-map dat #f))
    (hash-set! gensym-map dat (if cnt (add1 cnt) 0))
    (string->symbol (format "~a_~a" dat (or cnt 0))))
  (new-id! old))
(define (add-special-pfun name ptypes)
  (define name-datum (syntax->datum (ast-id-stxid name)))

  (define sid (cons name-datum (type->datum ptypes)))
  (define orig (hash-ref special-names sid #f))
  (or orig
      (let ([nn (datum->syntax (ast-id-stxid name) (new-name name-datum))])
        (hash-set! special-names sid nn)
        nn)))
(define (type->datum t)
  (match t
    [(type-bit) 'bit]
    [(type-int) 'int]
    [(type-idx) 'idx]
    [(type-tup ts ...) `(tup ,@(map type->datum ts))]
    [(type-seq d t) `(seq ,d ,(type->datum t))]
    [(type-fun (tps ...) (tvs ...) tto)
     `(-> ,(map type->datum tps) ,(map type->datum tvs) ,(type->datum tto))]
    [(? integer?) t]
    [t `(unkt ,t)]
    [(list ts ...) (map type->datum ts)]))
(define (ctype t)
  (type->datum t))
(define (cexpr e)
  (match e
    [(expr-bnd t ds ... b)
     `(bnd ,(map cdef ds) ,(cexpr b))]
    [(expr-cnd t (chk thn) ... els)
     `(cnd (,(map cexpr chk) ,(map cexpr thn)) ,(cexpr els))]
    [(expr-app t op (args ...))
     `(,(cexpr op) ,@(map cexpr args))]
    [(expr-tup t vals ...)
     `(tup ,@(map cexpr vals))]
    [(expr-lit t i)
     i]
    [(expr-seq-val t vals ...)
     `(seq ,@(map cexpr vals))]
    [(expr-seq-enm t f s e)
     `(enm ,(cexpr f) ,(cexpr s) ,(cexpr e))]
    [(expr-seq-laz t idx val)
     `(laz ,idx ,(cexpr val))]
    [(expr-tup-idx t v i)
     `(tup-idx ,(cexpr v) ,(cexpr i))]
    [(expr-seq-idx t v i)
     `(seq-idx ,(cexpr v) ,(cexpr i))]
    [(expr-seq-len t v)
     `(seq-len v)]
    [(expr-bnv t n)
     `(bnv ,n)]
    [(expr-lid t n)
     `(lid ,n)]
    [(expr-fvar t d)
     (cdef d)]
    [(expr-pvar t n)
     `(pvar ,n)]
    [(expr-frg t i)
     `(frg ,i)]
    [(expr-err t s)
     `(err ,s)]
    [(def-val n t v)
     `(def-var ,n)]
    [else `(unke ,e)]))
(define (cfun d)
  (match-define (def-cfun n t b) d)
  (define nn (add-special-pfun n t))
  (define old (hash-ref special-funs nn #f))
  (define (comp!)
    (define c `(fun ,n ,(ctype t) ,(cexpr b)))
    ;; (add-cfun c)
    (hash-set! special-funs nn c)
    )
  (unless old (comp!))
  nn)
(define (cdef d)
  (match d
    [(def-val n t v)
     `(val ,n ,(ctype t) ,(cexpr v))]
    [(def-cfun n t b)
     (define nn (cfun d))
     `(cfun ,nn)]
    [(def-pfun n (tps ...) t)
     ;; (define nn (pfun d))
     `(pfun ,n ,(map ctype tps))
     ]))
(define (ctest t)
  ;; (printf "ctext: ~a" t)
  (match t
    [(def-tst name typ v1 v2)
     `(tst ,(ctype typ) ,(cexpr v1) ,(cexpr v2))]))

(define (to-sham test-specs)
  (match-define (cons tests specs) test-specs)
  (pretty-print (map ctest tests))
  (pretty-print (map cdef specs))
  (pretty-print (unbox cfuns))
  (pretty-print special-names)
  (pretty-print special-funs)
  #`42)


(define require-syntax
  #`((require sham
              sham/jit
              sham/cryptol/prelude/sham
              (prefix-in ll- sham/llvm/ir))))

(define extra-syntax
  #`((module+ test
       (define built-sham-env (build-sham-env cryptol-module #:debug #t))
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-llvm-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       ;; (sham-env-optimize-llvm! built-sham-env #:opt-level 2)
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-opt-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       (define built-jit-env (initialize-jit built-sham-env))
       (define test-runner (jit-lookup-function built-jit-env 'run-tests))
       (printf "running-cryptol-tests:\n")
       (test-runner))))
