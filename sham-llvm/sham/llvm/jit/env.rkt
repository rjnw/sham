#lang racket

(provide (all-defined-out))

(struct llvm-jit-env [lenv jit-ref])
(struct llvm-mcjit-env llvm-jit-env [])
(struct llvm-orc-env llvm-jit-env [])
