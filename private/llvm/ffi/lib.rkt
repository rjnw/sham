#lang racket

(require
  racket/port)
(require ffi/unsafe)


(require srfi/13)

(provide llvm-lib)

(define llvm-version-string
  (let-values (((process out in err)
                (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--version")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (equal? (subprocess-status process) 0)
     (error 'llvm-config "Returned non zero exit code")))))

(define llvm-lib-path
  (let-values (((process out in err)
                (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--libdir")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (equal? (subprocess-status process) 0)
     (error 'llvm-config "Returned non zero exit code")))))

(define llvm-lib
  (let ((lib-name (string-append "libLLVM-" llvm-version-string)))
    (ffi-lib
      (case (system-type 'os)
        ((macosx) (build-path llvm-lib-path lib-name))
        ((unix) (string->path lib-name))
        ((windows) (string->path lib-name)))
      #:global? #t)))
