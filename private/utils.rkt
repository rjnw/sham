#lang racket/base

(require racket/splicing
         racket/list
         racket/string
         racket/system
         (for-syntax racket/base))

(provide (all-defined-out))

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))

(define sham-diagnose (make-parameter #f))
(define sham-debug (make-parameter #f))

(define (symbol-append s1 s2)
  (string->symbol
   (string-append (symbol->string s1)
                  (symbol->string s2))))

(define (const-true . a) #t)

(define-syntax (try/no-finally stx)
  (syntax-case stx (catch)
    [(_ exp ... (catch [e? rhs] ...))
     #'(with-handlers ([e? rhs] ...) exp ...)]
    [(_ exp ...) #'(let () exp ...)]))

(define-syntax (try stx)
  (syntax-case stx (finally)
    [(_ exp ... (finally fnl ...))
     #'(let ([th (thunk (begin fnl ...))])
         (with-handlers ([const-true (lambda (e) (th) (raise e))])
           (let ([ret (try/no-finally exp ...)])
             (th)
             ret)))]
    [(_ exp ...) #'(try/no-finally exp ...)]))

;; Run a shell command and return the stdout as a string.
(define (get-out-string process-str)
  (define out (open-output-string))
  (define l (process/ports out #f #f process-str))
  ((last l) 'wait)
  (string-trim (get-output-string out)))

(define (llvm-lhs v)
  (cond
    [(string? v) (substring (string-append v "value") 0 5)]
    [(symbol? v) (substring (string-append (symbol->string v) "value") 0 5)]
    [else "value"]))

(module+ test
  (require rackunit)
  (test-equal? "get-out-string basic test"
               (get-out-string "echo Test")
               "Test"))
