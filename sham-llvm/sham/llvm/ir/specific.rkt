#lang racket
(require (for-syntax racket/syntax syntax/parse racket/match racket/list)
         sham/llvm/ir/simple
         sham/llvm/ir/internals)

(provide (except-out (all-defined-out)
                     define-ref-types))

(define-syntax (define-ref-types stx)
  (define stars (build-list 4 (λ (j) (make-string j #\*))))
  (define (do-type t)
    (let rec ([prev #f]
              [l (map (λ (i) (format-id stx "~a~a" t i)) stars)])
      (match* (prev l)
        [(#f (cons curr next))
         (cons #`(define #,curr (type-ref (quote #,curr))) (rec curr next))]
        [(p (cons curr next))
         (cons #`(define #,curr (type-pointer #,p)) (rec curr next))]
        [(p empty) empty])))
  (syntax-case stx ()
    [(_ types-reference)
     #`(begin #,@(append-map do-type (syntax-local-value #'types-reference)))]))

(define-ref-types basic-types)

(define-syntax (define-basic-ops stx)
  (define (do-op op)
    (with-syntax ([op-syn (datum->syntax stx op)]
                  [op-name (format-id stx "op-~a" op)])
      #`(define-syntax (op-name stx)
          (syntax-parse stx
            [(_ (~optional (~seq #:flags flags) #:defaults ([flags #'#f])) result args)
             (syntax/loc stx (inst-op result (quote op-syn) flags args))]))))
  (syntax-case stx ()
    [(_ ops-reference)
     #`(begin #,@(map do-op (syntax-local-value #'ops-reference)))]))

(define-basic-ops basic-ops)

(define-syntax (define-int-const-alias stx)
  (define (do-size s)
    (list #`(define (#,(format-id stx "ui~a" s) v) (val-ui v #,(format-id stx "i~a" s)))
          #`(define (#,(format-id stx "si~a" s) v) (val-si v #,(format-id stx "i~a" s)))))
  (syntax-case stx ()
    [(_ size-reference)
     #`(begin #,@(append-map do-size (syntax-local-value #'size-reference)))]))
(define-syntax (define-float-const-alias stx)
  (define (do-size s)
    #`(define (#,(format-id stx "fl~a" s) v) (val-fl v #,(format-id stx "f~a" s))))
  (syntax-case stx ()
    [(_ size-reference)
     #`(begin #,@(map do-size (syntax-local-value #'size-reference)))]))

(define-int-const-alias int-widths)
(define-float-const-alias float-widths)
