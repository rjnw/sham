#lang racket

(provide (all-defined-out))

(struct sham-module [ast ll-ast externals])

(struct sham-env [mod ll-env])

(struct sham-value [uintptr ast-type rkt-type])

(struct sham-jit-env [orig-env ll-env value-ref])
