#lang racket

(require "spec.rkt"
         "pattern.rkt"
         syntax/parse
         (prefix-in rt: "runtime.rkt"))

(provide term-match-expander)

(define (pattern-expander stx pat)
  (define parsed (parse-with-pattern pat stx))
  (let rec ([ps parsed])
    (match ps
      [`(multiple (,args ...) ,pat) #`(vector #,@(map rec (reverse args)))]
      [`(repeat (,args ...) ,pat) #`(list #,@(map rec (reverse args)))]
      [`(single ,arg ,pat) #`#,arg]
      [`(ooo ,is ,k ,p) #`#,is]
      [else (error 'sham/sam/internal "could not match parsed syntax ~a in pattern ~a\n" ps pat)])))

(define (match-group-args gargs stxs)
  (match* (gargs stxs)
    [('() ss) (values '() ss)]
    [((cons gs grs) (cons ss srs))
     (define-values (gr sr) (match-group-args grs srs))
     (values (cons ss gr) sr)]
    [(_ _) (error 'sham/sam "not enough arguments for common group args")]))

(define (term-match-expander tt stx)
  (match-define (rt:term-type rt mt ss ts) tt)
  (printf "match-transformer: ~a\n" stx)
  (syntax-parse stx
    [(_ (~optional (~seq (~datum #:md) md)) args ...)
     (match ss
       [(ast:node ids ninfo nargs pat)
        (define gs (find-node-group ss ts))
        (define gargs (full-group-args gs ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        (define nargs-stx (pattern-expander #`(#,@rest-stx) pat))
        #`(#,(ast:id-gen ids) (~? md _) (vector #,@gargs-stx) #,nargs-stx)]
       [(ast:group ids ginfo prnt gargs nodes)
        (define gargs (full-group-args ss ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        ;; (error 'sham:sam "todo match expander for group types: ~a" (ast:id-orig ids))
        #`(#,(ast:id-gen ids) (~? md _) (vector #,@gargs-stx) _)
        ])]))

(module+ test
  (require rackunit))
