#lang racket

(require "spec.rkt"
         "pattern.rkt"
         (prefix-in rt: "runtime.rkt"))

(provide term-match-expander)

(define (pattern-expander stx pat)
  (map-with-pattern pat stx))

(define (term-match-expander tt stx)
  (printf "~a ~a\n" stx tt)
  (match-define (rt:term-type rt mt ss ts) tt)
  (syntax-case stx ()
    [(_ args ...)
     (match ss
       [(ast:node (cons id idt) fid nargs pat ninfo)
        #`(#,idt md gargs #,(pattern-expander #`(args ...) pat))]
       [(ast:group idt fid prnt gargs nodes ginfo)
        (error 'sham:sag "todo match expander for group types: ~a" (car idt))])]))

(module+ test
  (require rackunit)
  ;; (define pt
  ;;   (ast:pat:multiple
  ;;    (list
  ;;     (ast:pat:datum `letrec)
  ;;     (ast:pat:multiple
  ;;      (list
  ;;       (ast:pat:repeat
  ;;        (ast:pat:multiple
  ;;         (list (ast:pat:single #'ids) (ast:pat:single #'vals)))
  ;;        (list #f))))
  ;;     (ast:pat:single #'e))))
  ;; (vector (vector (list (vector a b) \.\.\.)) e)
  ;; (pretty-print (pattern-expander #`(`letrec (([a b] (... ...))) e) pt))
  (define p2
    (ast:pat:multiple
     (list
      (ast:pat:datum '+)
      (ast:pat:repeat (ast:pat:single #f #`e) (list #f)))))
  (pretty-print (pattern-expander #`((e (... ...))) p2)))
