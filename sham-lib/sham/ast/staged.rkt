#lang racket

(require "core.rkt")

(define (partial-app f . args-vals)
  (match-define (sham:def:function info id args arg-types ret-type body-stmt) f)
  (define arg-syms (map car args-vals))
  (define arg-vals (map cdr args-vals))
  (define-values (gots tgots ngots ntgots)
    (for/fold ([gots '()]
               [tgots '()]
               [ngots '()]
               [ntgots '()])
              ([farg args]
               [ftyp arg-types])
      (if (ormap (curry member farg) arg-syms)
          (values (append gots (list farg))
                  (append tgots (list ftyp))
                  ngots ntgots)
          (values gots tgots
                  (append ngots (list farg))
                  (append ntgots (list ftyp))))))
  (define nid (gensym id))
  (define nfunc
    (sham:def:function info nid ngots ntgots ret-type
                       (sham:ast:expr:let gots tgots body-stmt (sham:ast:expr:void))))
  (values nid nfunc))
