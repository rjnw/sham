#lang racket

(provide (all-defined-out))
(struct sham-env [ast llvm-env externals])

(struct sham-value [uintptr ast-type rkt-type])
(struct sham-jit-env [orig ljit value-ref])
