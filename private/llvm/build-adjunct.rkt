#lang racket

(require racket/system
         dynext/compile
         ffi/unsafe
         srfi/13
         "ffi/lib.rkt")

(provide adjunct-lib)

(define adjunct-so-path
  (build-path (collection-path "sham") "private" "llvm" (string-append "adjunct" (bytes->string/utf-8 (system-type 'so-suffix)))))

(define adjunct-file-path
  (build-path (collection-path "sham") "private" "llvm" "adjunct.cpp"))

(define llvm-c-flags
  (let-values (((process out in err)
                (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--cflags")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (equal? (subprocess-status process) 0)
     (error 'llvm-config "Returned non zero exit code")))))
(define (compile-adjunct)
  (parameterize ([current-extension-compiler-flags (cons "--shared" (string-split llvm-c-flags))])
    (compile-extension #f adjunct-file-path adjunct-so-path '())))

(define adjunct-lib
  (begin (unless (file-exists? adjunct-so-path)
           (compile-adjunct))
         (ffi-lib adjunct-so-path)))
