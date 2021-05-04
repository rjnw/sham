#lang racket

(require racket/syntax)
(require "private/utils.rkt")

(provide (all-defined-out))

(struct ast:format (top? top-sep mid-cnt mid-sep bot-sep) #:prefab)
(struct ast:format:custom (before-first after-first before-last after-last f) #:prefab)
(define default-ast-format (ast:format #t #': #t #': #':))

(define ast:sep/c (or/c symbol? syntax/c string?))
(define ast:format/c (struct/c ast:format (or/c #f #t) ast:sep/c (or/c #f #t natural-number/c) ast:sep/c ast:sep/c))

(define (ast-format raw)
  (let rec ([frmt raw])
    (match frmt
      [#f default-ast-format]
      [(? ast:format?) frmt]
      [(? identifier?) (rec (syntax-local-value frmt #f))]
      [(list t? ts mc ms bs) (ast:format t? ts mc ms bs)]
      [(? syntax?) (rec (syntax->datum frmt))]
      [frmt (error 'sham/sam "unknown ast formatter ~a" frmt)])))

(define (join-to-syntax tree (ctxt #f))
  (define (->string d) (symbol->string (->symbol d)))
  (format-id ctxt "~a" (string-join (map ->string (filter identity (flatten tree))) "")))

(define (do-normal top? top-sep mid-cnt mid-sep bot-sep tid mids bid)
  (define mi (if mid-cnt
                 (add-between (if (exact-nonnegative-integer? mid-cnt) (take mids mid-cnt) mids) mid-sep)
                 '()))
  (define ti (if top? (list tid top-sep) '()))
  (define pre (flatten (append ti mi)))
  (list pre (if (empty? mi) '() bot-sep) bid))

(define (do-custom normal cstm)
  (define (get-info c)
    (if (ast:format:custom? c) c (assoc-default cstm custom-format-info)))
  (if cstm
      (match-let ([(ast:format:custom bf af bl al f) (get-info cstm)])
        ((if f f identity)
         (match normal
           [(list a) a]
           [(list a b) (list bf a af b)]
           [(list a b ... c) (list bf a af b bl c al)])))
      normal))

(define (ast-format-id tid mids bid frmt (cfrmt #f))
  (match-define (ast:format t? ts mc ms bs) frmt)
  (join-to-syntax (do-custom (do-normal t? ts mc ms bs tid mids bid) cfrmt) bid))

(define custom-format-info `((0 . #f)
                             (rkt-struct . ,(ast:format:custom "struct-" '() '() '() #f))
                             (spec . ,(ast:format:custom "#%spec-" '() '() '() #f))))

(module+ test
  (require rackunit)
  (check-equal? (->symbol (ast-format-id #'l (list #'g0 #'g1) #'n default-ast-format))
                'l:g0:g1:n))
