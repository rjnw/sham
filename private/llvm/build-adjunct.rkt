#lang racket

(require racket/system
         dynext/compile
         ffi/unsafe)

(provide adjunct-lib)

(define adjunct-so-path
  (build-path (collection-path "sham") "private" "llvm" (string-append "adjunct" (bytes->string/utf-8 (system-type 'so-suffix)))))
(define adjunct-file-path
  (build-path (collection-path "sham") "private" "llvm" "adjunct.cpp"))

(define (compile-adjunct)
  (parameterize ([current-extension-compiler-flags (list "-shared" "-fPIC" "-O2" )])
    (compile-extension #f adjunct-file-path adjunct-so-path '())))

(define adjunct-lib
  (begin (unless (file-exists? adjunct-so-path)
           (compile-adjunct))
         (ffi-lib adjunct-so-path)))
