#lang racket

(require sham/ir/env
         sham/ir/ast/core
         sham/llvm/ir/ast
         sham/llvm/ir/dump)

(provide (all-defined-out))

(define (sham-dump-ir e)
  (match e
    [(sham:def:module md name defs)
     (printf "sham-module: ~a\n" name)
     (for ([d defs])
       (match d
         [(sham:def _ id) (printf "~a: \n" id) (pretty-print d) (newline)]
         [(llvm:def _ id) (printf "~a: \n" id) (pretty-print d) (newline)]))]
    [(sham-env (sham-module sh-ast ll-ast extrs) ll-env md) (sham-dump-ir sh-ast)]
    [(sham-module sh-ast ll-ast extrs) (sham-dump-ir sh-ast)]
    [else (error 'sham:dump:ir "unknown sham-ir ~a" e)]))

(define (sham-dump-llvm e)
  (match e
    [(llvm:def:module md name defs)
     (printf "sham-llvm-module: ~a\n" name)
     (for ([d defs])
       (match d
         [(llvm:def _ id) (printf "~a: \n" id) (pretty-print d) (newline)]))]
    [(sham-env (sham-module sh-ast ll-ast extrs) ll-env md) (sham-dump-llvm ll-ast)]
    [(sham-module sh-ast ll-ast extrs) (sham-dump-llvm ll-ast)]
    [else (error 'sham:dump:llvm "unknown sham-llvm env ~a" e)]))

(define (sham-dump-llvm-ir s-env)
  (dump-llvm-ir (sham-env-ll-env s-env)))

(define (sham-write-llvm-ir s-env fname)
  (write-llvm-ir (sham-env-ll-env s-env) fname))
