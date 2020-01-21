#lang racket

(require racket/system
         dynext/compile
         ffi/unsafe
         racket/runtime-path
         "ffi/lib.rkt"
         "llvm-config.rkt")

(provide
 adjunct-lib
 reset-adjunct-unavailable?!
 adjunct-available?)

(define-runtime-path adjunct-so-path
  (bytes->string/utf-8
   (bytes-append #"adjunct" (system-type 'so-suffix))))

(define-runtime-path adjunct-file-path "adjunct.cpp")
(define-runtime-path adjunct-not-available-path ".adjunct-not-available")


(define (compile-adjunct)
  ;;builds a parameterization barier that should "reset" the compiler
  (define flags
    (list*
     "--shared"
     "--std=c++11"
     "-fPIC"
     (string-split (llvm-config "--cflags" "--libs" "--ldflags"))))
  (parameterize ([current-extension-compiler (current-extension-compiler)]
                 [current-extension-compiler-flags flags])
    (define cpp? (getenv "CPP"))
    (when cpp? (current-extension-compiler (find-executable-path cpp?)))
    (compile-extension #f adjunct-file-path adjunct-so-path '())))

(define (adjunct-is-unavailable!)
  (close-output-port (open-output-file adjunct-not-available-path #:exists 'append)))

(define (reset-adjunct-unavailable?!)
  (delete-file adjunct-not-available-path))

(define (adjunct-unavailable?)
  (file-exists? adjunct-not-available-path))

(define adjunct-lib
  (begin
    (unless (or (file-exists? adjunct-so-path)
                (adjunct-unavailable?))
      (with-handlers
        ([exn?
          (lambda (e)
            (adjunct-is-unavailable!)
            (display
             (append
              "sham: Couldn't build adjunct file, continuing without.\n"
              "Some functions will not be available.\n")))])
        (compile-adjunct)))
    (cond
      [(file-exists? adjunct-so-path) (ffi-lib adjunct-so-path)]
      [else #f])))

(define (adjunct-available?)
  (not (not adjunct-lib)))

(module+ test
  (require rackunit)
  (test-true "adjunct builds" (adjunct-available?)))
