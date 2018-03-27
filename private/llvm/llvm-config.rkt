#lang racket/base

(provide llvm-config)
(require racket/string
         "../lib.rkt")


(define llvm-config-name
  (let ([cmd (or (getenv "LLVM_CONFIG") "llvm-config")])
    (unless (find-executable-path cmd)
      (error 'sham/llvm-config-name
             (string-join
              '("Couldn't find llvm-config."
                "Consider seting LLVM_CONFIG enviroment variable.")
              "\n")))
    cmd))

(define (llvm-config . a)
  (get-out-string
   (string-join (cons llvm-config-name a) " ")))
