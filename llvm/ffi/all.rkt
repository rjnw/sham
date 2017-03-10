#lang racket/base


(define-syntax-rule (reprovide path ...)
 (begin
   (require path ...)
   (provide (all-from-out path ...))))

(reprovide
 "lib.rkt"
 "define.rkt"
 "ctypes.rkt"
 "initialize.rkt"
 "execution-engine.rkt"
 "pass-manager.rkt"
 "transforms.rkt"
 "analysis.rkt"
 "bit-reader.rkt"
 "bit-writer.rkt"
 "core.rkt"
 "core-instructions.rkt"
 "core-types.rkt"
 "core-values.rkt"
 "instruction-builder.rkt"
 "target-info.rkt")
