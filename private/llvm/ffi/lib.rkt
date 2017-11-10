#lang racket

(require

 ffi/unsafe)

(provide llvm-lib)

(define (get-out-string process-str)
  (let* ([out (open-output-string)]
         [l (process/ports out #f #f process-str)])
    ((last l) 'wait)
    (string-trim (get-output-string out))))

(define llvm-version-string (get-out-string "llvm-config --version"))
(define llvm-lib-path (get-out-string "llvm-config --libdir"))

(define llvm-lib
  (let ((lib-name (string-append "libLLVM-" llvm-version-string)))
    (ffi-lib
      (case (system-type 'os)
        ((macosx) (build-path llvm-lib-path lib-name))
        ((unix) (string->path lib-name))
        ((windows) (string->path lib-name)))
      #:global? #t)))
