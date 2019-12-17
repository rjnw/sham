#lang racket

(define-syntax-rule (reprovide path ...)
 (begin
   (require path ...)
   (provide (all-from-out path ...))))

(reprovide
 "llvm/ffi/all.rkt"
 "llvm/adjunct.rkt"
 "llvm/pass-table.rkt")
