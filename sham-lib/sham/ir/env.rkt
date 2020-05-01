#lang racket

(provide (all-defined-out))

(struct sham-module [ast ll-ast externals])

(struct sham-env [mod ll-env info])
