#lang racket

(require syntax/parse
         (prefix-in rt: "../../runtime/transform.rkt")
         racket/stxparam
         (for-syntax racket/syntax))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-for-syntax cmplr-input-stxid #'transform-input)
(define-syntax-parameter this-ast (make-rename-transformer cmplr-input-stxid))

(define-for-syntax result-attribute-stxid #'result)

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

(define-for-syntax (id-cmplr-expander cmplrf stx)
  (syntax-case stx (quote)
    [(_ name (quote kind))
    #`(~and (~var name id)
            (~bind [#,(format-id #`name "~a.~a" #`name result-attribute-stxid)
                    (#,cmplrf #'name)])) ]))
(define-syntax ~id-ref
  (pattern-expander
   (lambda (stx) (id-cmplr-expander #`rt:compile-identifier-ref stx))))

(define-syntax ~id-def
  (pattern-expander
   (lambda (stx) (id-cmplr-expander #`rt:compile-identifier-def stx))))
