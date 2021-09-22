#lang racket

(require syntax/parse
         (for-syntax racket/syntax))

(provide (all-defined-out))

(define-for-syntax (paren-shape-expander shape stx)
  (syntax-case stx ()
       [(_ pat)
        (with-syntax ([(lst-id) (generate-temporaries '(lst))])
          #`(~and lst-id
                  (~fail #:unless (equal? (syntax-property #'lst-id 'paren-shape) #,shape) "incorrect paren-shape")
                  pat))]))

(define-syntax ~paren-shape-normal
  (pattern-expander
   (lambda (stx) (paren-shape-expander #f stx))))
(define-syntax ~paren-shape-square
  (pattern-expander
   (lambda (stx) (paren-shape-expander #\[ stx))))
(define-syntax ~paren-shape-curly
  (pattern-expander
   (lambda (stx) (paren-shape-expander #\{ stx))))
