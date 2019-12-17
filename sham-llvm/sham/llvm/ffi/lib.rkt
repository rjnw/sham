#lang racket

(require
 "../llvm-config.rkt"
 ffi/unsafe)

(provide llvm-lib)

(define llvm-version-string (llvm-config "--version"))
(define llvm-lib-path (llvm-config "--libdir"))

(define llvm-lib
  (let ((lib-name (string-append "libLLVM-" llvm-version-string)))
    (ffi-lib
      (case (system-type 'os)
        ((macosx unix) (build-path (llvm-config "--libfiles")))
        ((windows) (string->path lib-name)))
      #:global? #t)))
