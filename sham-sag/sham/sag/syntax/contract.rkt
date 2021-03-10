#lang racket

(require "spec.rkt"
         (prefix-in rt: "runtime.rkt"))

(provide term-type-contract)

(define ((do-pat for-id) p)
  (match p
    [(ast:pat:single check sid) (if check #`(flat-contract #,check) (for-id sid))]
    [(ast:pat:datum syn) #f]
    [(ast:pat:multiple ps) #`(vector/c #,@(filter-map (do-pat for-id) ps))]
    [(ast:pat:repeat pr k-or-more) #`(listof #,((do-pat for-id) pr))]))

(define (term-type-contract tt)
  (match-define (rt:term-type rt mt ss ts) tt)
  (define (for-id id)
    any/c)
  (match ss
    [(ast:node idt fid nargs pat ninfo)
     ((do-pat for-id) pat)]
    [(ast:group idt fid prnt gargs nodes ginfo)
     (error 'sham:sag "todo contract for group types: ~a" (car idt))]))
