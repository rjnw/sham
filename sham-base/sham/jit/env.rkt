#lang racket

(provide (all-defined-out))

(struct sham-jit-env [senv ll-jit-env value-refs])

(struct sham-mcjit-env sham-jit-env [])
(struct sham-orc-env sham-jit-env [])

(struct sham-jit-value [uintptr rkt-value])
