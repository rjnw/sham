#lang racket

(require racket/system
         dynext/compile
         ffi/unsafe
         racket/runtime-path
         "ffi/lib.rkt"
         "../lib.rkt"
         "llvm-config.rkt")

(provide adjunct-lib)

(define-runtime-path adjunct-so-path
  (bytes->string/utf-8
   (bytes-append #"adjunct" (system-type 'so-suffix))))

#;
(define adjunct-so-path
  (build-path (collection-path "sham") "private" "llvm" (string-append "adjunct" (bytes->string/utf-8 (system-type 'so-suffix)))))

(define-runtime-path adjunct-file-path "adjunct.cpp")

#;(build-path (collection-path "sham") "private" "llvm" "adjunct.cpp")

(define (compile-adjunct)
  ;;builds a parameterization barier that should "reset" the compiler
  (define flags
    (list*
     "--shared"
     "--std=c++11"
     (string-split (llvm-config "--cflags" "--libs" "--ldflags"))))
  (parameterize ([current-extension-compiler (current-extension-compiler)]
                 [current-extension-compiler-flags flags])
    (define cpp? (getenv "CPP"))
    (when cpp? (current-extension-compiler (find-executable-path cpp?)))
    (compile-extension #f adjunct-file-path adjunct-so-path '())))

(define adjunct-lib
  (begin
    (unless (file-exists? adjunct-so-path)
      (compile-adjunct))
    (ffi-lib adjunct-so-path)))

(module+ test
 (compile-adjunct))
