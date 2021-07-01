#lang racket
(require (for-syntax syntax/parse
                     racket/match
                     racket/pretty
                     "syntax/spec.rkt"
                     (submod "syntax/private/spec.rkt" compiler)
                     (submod "syntax/private/syntax-class.rkt" compiler))
         racket/stxparam)
(require (for-template (prefix-in rkt: racket)))
(provide (all-defined-out))

(define-syntax-parameter compile (make-rename-transformer #'rkt:compile))
(define-syntax-parameter ^ (make-rename-transformer #'rkt:^))

(begin-for-syntax
  (define (cmplr-pattern-with-path f val pat)
    (let rec
        ([path null]
         [val val]
         [pat pat])
      (match pat
        [(cmplr:pat:ooo p cnt) (f rec `(ooo ,p ,cnt ,path) val pat)]
        [(cmplr:pat:op op body) (f rec `(op ,op ,body ,path) val pat)]
        [(cmplr:pat:app rator rands) (f rec `(app ,rator ,rands ,path) val pat)]
        [(? syntax?) (f rec `(syn ,pat ,path) val pat)]))))

(define-syntax (define-compiler stx)
  (define binding-ops (list #`^))
  (define body-ops (list #`compile #`with))
  (define (func s) (λ (sop) (free-identifier=? sop s)))
  (syntax-parse stx
    [(_ (~var s (compiler-spec #:legal-ops (cons (map func binding-ops) (map func body-ops)))))
     (define spec (attribute s.spec))
     (match-define (cmplr header groups) spec)
     (match-define (cmplr:header compiler-id top-args top-type) header)
     (match-define (cmplr:type tfrom tto) top-type)
     (define-values (from-spec-value ff) (syntax-local-value/immediate tfrom))
     (define-values (to-spec-value tf) (syntax-local-value/immediate tto))
     (let ([intrinsic-args (map car top-args)])
       (define (do-group grp)
         (match-define (cmplr:group name type nodes info) grp)
         (define (do-node nde)
           (match-define (cons bind body) nde)
           (define (expand-bind-pattern pat)
             (let rec ([p pat]
                       [bvars null]
                       [ooo-depth 0]))
             (match p
               [(cmplr:pat:ooo pooo cnt) (rec pooo bvars (add1 ooo-depth))]
               [(cmplr:pat:op op body)
                (match (syntax->datum op)
                  ['^
                   (match (car body)
                     [(? identifier?) (car (cons (car body)
                                                 `(to-compile ,ooo-depth)))])])]
               [(cmplr:pat:app rator rands) #`(#,rator #,@(map expand-bind-pattern rands))]
               [(? syntax?) pat]))
           (define (expand-body-pattern pat)
             (match pat
               [(cmplr:pat:ooo pat cnt) (expand-body-pattern pat)]
               [(cmplr:pat:op op body)
                (match (syntax->datum op)
                  ['with (do-with-op body)])]
               [(cmplr:pat:app rator rands) #`(#,rator #,@(map expand-bind-pattern rands))]
               [(? syntax?) pat])  )
           (cons (expand-bind-pattern bind)
                 (expand-body-pattern body)))
         #`(define (#,name #,@intrinsic-args)
             (match #,input
               #,@(map do-node nodes))))
       #`(define (#,hid input #,@args)
           #,@(map do-groups groups)
           #f))

     ;; (let ([header-spec (attribute header.spec)]
     ;;       [type-spec (attribute type.spec)]
     ;;       [groups-spec (attribute groups.spec)])
     ;;   (match-define (cmplr:header hid args) header-spec)
     ;;   (printf "define-compiler: ~a\n" hid)
     ;;   (match-define (cmplr:type tfrom tto) type-spec)
     ;;   (define-values (from-spec-value ff) (syntax-local-value/immediate tfrom))
     ;;   (define-values (to-spec-value tf) (syntax-local-value/immediate tto))
     ;;   (printf "from: \n") (pretty-print (pretty-spec from-spec-value)) (newline)
     ;;   (define (parse-pattern stx)
     ;;     (syntax-parse stx
     ;;       [(id:id args ...) ]))
     ;;   (pretty-print
     ;;    (syntax->datum
     ;;     #`(define (#,hid input #,@args)
     ;;         #,@(for/list ([group groups-spec])
     ;;              (match-define (cmplr:group id type nodes info) group)
     ;;              #`(define (#,id input #,@(map car args))
     ;;                  (match input
     ;;                    #,@nodes)))
     ;;         #f)))
     ;;   #`42)
     ]))
