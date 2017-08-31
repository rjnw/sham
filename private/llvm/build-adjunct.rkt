#lang racket

(require racket/system
         dynext/compile)

(provide adjunct-so-name
         compile-adjunct)

(define adjunct-so-name
  (string-append "adjunct" (bytes->string/utf-8 (system-type 'so-suffix))))

(define (compile-adjunct)
  (compile-extension #f "adjunct.cpp" adjunct-so-name '()))
