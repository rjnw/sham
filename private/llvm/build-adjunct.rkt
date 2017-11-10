#lang racket

(require racket/system
         dynext/compile
         ffi/unsafe
         "ffi/lib.rkt")

(provide adjunct-lib)

(define (get-out-string process-str)
  (let* ([out (open-output-string)]
         [l (process/ports out #f #f process-str)])
    ((last l) 'wait)
    (string-trim (get-output-string out))))

(define adjunct-so-path
  (build-path (collection-path "sham") "private" "llvm" (string-append "adjunct" (bytes->string/utf-8 (system-type 'so-suffix)))))

(define adjunct-file-path
  (build-path (collection-path "sham") "private" "llvm" "adjunct.cpp"))

(define llvm-c-flags (get-out-string "llvm-config --cflags"))

(define (compile-adjunct)
  (parameterize ([current-extension-compiler-flags (cons "--shared" (string-split llvm-c-flags))])
    (compile-extension #f adjunct-file-path adjunct-so-path '())))

(define adjunct-lib
  (begin (unless (file-exists? adjunct-so-path)
           (compile-adjunct))
         (ffi-lib adjunct-so-path)))
