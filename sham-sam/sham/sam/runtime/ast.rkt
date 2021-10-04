#lang racket
(require "generics.rkt"
         "props.rkt")

(provide (all-defined-out))

(struct ast [md])
(struct ast:group ast [args])

(define (print-ast ast port mode)
  (pretty-print
   (let rec ([a ast])
     (match a
       [(ast:term md gargs args)
        (append (list (object-name a))
                ;; (if md (list md) (list))
                (if (vector-empty? gargs) (list) (list (rec gargs)))
                (if (vector-empty? args) (list) (vector->list (rec args))))
        ;; `(,(object-name a) ,(rec gargs) ,(rec args))
        ]
       [(ast:id md stxid) (syntax->datum stxid)]
       [(? vector? v) (vector-map rec v)]
       [(? list? v) (map rec v)]
       [(? (or/c string? number? symbol?) v) v]
       [else (pretty-format a)]))
   port))

(struct ast:term ast:group [args]
  #:methods gen:term:fold
  [(define (gfold ff f v)
     (match-define (ast:term md gas tas) v)
     (define ngas ((gfold-rec-vl ff f) gas))
     (define ntas ((gfold-rec-vl ff f) tas))
     (cond [(ff v) => (lambda (ff^) (ff^ ngas ntas))]
           [(has-ast-constructor? v) (((get-ast-constructor v)) md ngas ntas)]))]
  #:methods gen:term:map
  [(define (gmap f v)
     (match-define (ast:term md gas tas) v)
     ((if (has-ast-constructor? v) (((get-ast-constructor v))) ast:term)
      md
      ((gmap-rec-vl f) gas)
      ((gmap-rec-vl f) tas)))]
  #:methods gen:custom-write
  [(define write-proc print-ast)])

(struct ast:id ast [stxid]
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (print (syntax->datum (ast:id-stxid v)) port))])
