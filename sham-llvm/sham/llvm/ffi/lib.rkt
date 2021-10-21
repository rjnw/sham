#lang racket

(require
 "../llvm-config.rkt"
 ffi/unsafe
 setup/dirs)

(provide llvm-lib)

(define llvm-version-string (llvm-config "--version"))
(define llvm-lib-path (llvm-config "--libdir"))
(define llvm-major-version-string
  (let ([matches (regexp-match #px"^([0-9]+)\\." llvm-version-string)])
    (if matches
        (list-ref matches 1)
        #f)))

(define llvm-lib
  (cond
    [(getenv "LLVM_LIB_PATH") => (λ (path) (ffi-lib (build-path path)))]
    [else
     (define files-ls (string-split (llvm-config "--libfiles")))
     (cond
       [(= (length files-ls) 1) (ffi-lib (build-path (car files-ls)))]
       [else
        (ffi-lib
         (build-path "libLLVM")
         (list llvm-major-version-string #f)
         #:get-lib-dirs
         (case (system-type 'os)
           [(macosx unix)
            (lambda () (list llvm-lib-path))]
           [(windows) get-lib-search-dirs]))])]))
