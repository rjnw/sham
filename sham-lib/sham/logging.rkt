#lang racket/base

(define-logger sham)

(require
 (for-syntax racket/base)
 racket/logging
 racket/port
 syntax/srcloc
 syntax/location)

(provide sham-logger debug)

(define (debug-aux ls)
  (define (aux p _)
    (define expression (car p))
    (define value (cdr p))
    (cond
      ;; If the expression was just `,something then print the value of something
      ;; allows you to add names to debug statements like this
      #;(debug 'foo-returned value)
      [(and (pair? expression)
            (eq? 'quote (car expression))
            (pair? (cdr expression)))
       (printf "    ~a\n" (cadr expression))]
      [(string? expression) (printf "    ~a\n" expression)]
      [else (printf "    ~a=~v\n" expression value)]))
  (void (foldl aux (void) ls)))

(define-syntax (debug stx)
  (syntax-case stx (off std)
    [(_ off e* ... e) #'(void)]
    [(_ std e* ... e)
     #`(begin
         (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
         (debug-aux `((e* . ,e*) ... (e . ,e))))]
    [(_ e* ... e)
     #`(log-sham-debug
        (with-output-to-string
          (lambda ()
            #,(syntax/loc stx (debug std e* ... e)))))]
    [other (error 'debug "invalid syntax")]))
