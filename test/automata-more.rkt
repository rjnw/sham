#lang racket

(require rcf/ast
         sham
         sham/ast-utils
         sham/jit-utils
         racket/syntax
         (for-syntax syntax/parse racket/pretty racket/set))

(define fsa-module
  (create-empty-sham-module "module" (module-info-add-late-pass (empty-module-info) 'AlwaysInliner)))
(current-sham-module fsa-module)
(define finfo (function-info-add-attributes (empty-function-info) 'alwaysinline))

(define (ri64 i) (or^ (shl (ui64 i) (ui64 1)) (ui64 1)))
(define (build-mores len)
  (define names (for/list ([i len]) (format-symbol "more-~a" i)))
  (cons
   (sham-function #:info finfo
    (,(list-ref names (sub1 len)) (inp : i64*) : i64)
    (switch^ (load (gep^ inp (ui64 len)))
             [(ri64 3) (return (ui64 1))]
             (return (ui64 0))))
   (for/list ([i (in-range (sub1 len))]
              [name names]
              [next (cdr names)])
     (sham-function #:info finfo
      (,name (inp : i64*) : i64)
      (switch^ (load (gep^ inp (ui64 i)))
               [(ri64 1) (return (app^ (rs next) inp))]
               [(ri64 2) (return (app^ (rs next) inp))]
               (return (ui64 0)))))))

(map (curry add-to-sham-module! fsa-module) (build-mores 3))
(parameterize ([compile-options (list  'dump 'verify)])
  (compile-sham-module!
   fsa-module
   #:opt-level 3))
