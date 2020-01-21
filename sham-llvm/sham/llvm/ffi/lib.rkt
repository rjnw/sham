#lang racket

(require
 "../llvm-config.rkt"
 ffi/unsafe
 setup/dirs)

(provide llvm-lib)

(define llvm-version-string (llvm-config "--version"))
(define llvm-lib-path (llvm-config "--libdir"))

(define llvm-lib
  (ffi-lib
   (build-path "libLLVM")
   (list (llvm-config "--version") #f)
   #:get-lib-dirs
   (case (system-type 'os)
     [(macosx unix)
      (lambda () (list (llvm-config "--libdir")))]
     [(windows) get-lib-search-dirs])
   #:global? #t))
