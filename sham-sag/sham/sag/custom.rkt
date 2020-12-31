#lang racket

(require
 (for-syntax
  "generics.rkt"
  syntax/parse
  racket/syntax
  racket/pretty)
 "runtime.rkt")

(provide (all-defined-out))


(begin-for-syntax

  #;(struct ast-generic-map-builder [aid as]
      #:methods gen:ast-builder
      [(define (build-group-methods ab fmt gs)
         #f)
       (define (build-group-generics ab fmt gs) #f)
       (define (build-node-methods ab fmt gs ns)
         (let* ([ast-spec (ast-generic-map-builder-as ab)]
                [nid ]
                [pargs (group-args ast-spec gs)]
                [nargs (node-args ns)]
                [full-args (append (map car pargs) nargs)])
           (list
            #`#:methods #`gen:term
            (with-syntax ([(args ...) full-args])
              #`((define (gmap-t ff f v)
                   (match v [(#,nid args ...)
                             ((ff v) (f args) ...)])))))))])

  #;(struct ast-map-builder [ast-id ast-spec]
      #:methods gen:ast-builder
      [(define (build-group-methods ab fmt gs)
         #f)
       (define (build-group-generics ab fmt gs)
         (define ast-spec (ast-map-generic-ast-spec ab))
         (define gid (format-group-id fmt gs))
         (define generic-id (format-id #f "mapg-~a" gid))
         (define generic-map-id (format-id #f "map-~a" gid))
         (define generic-map-fs
           (map (λ (g) (format-id #f "f-~a" (format-group-id fmt g)))
                (cond [(ast-groups ast-spec)]
                      [else (hash-values (ast-groups ast-spec))])))
         (list #`(define-generics #,generic-id
                   (#,generic-map-id #,@generic-map-fs #,generic-id))))
       (define (build-node-methods ab fmt gs ns)
         (define ast-spec (ast-map-generic-ast-spec ab))
         (define gid (format-group-id fmt gs))
         (define nid (format-node-id fmt gs ns))
         (define generic-id (format-id #f "mapg-~a" gid))
         (define generic-map-id (format-id #f "map-~a" gid))
         (define generic-map-fs
           (map (λ (g) (format-id #f "f-~a" (format-group-id fmt g)))
                (cond [(ast-groups ast-spec)]
                      [else (hash-values (ast-groups ast-spec))])))
         (define pargs (group-args ast-spec gs))
         (define nargs (node-args ns))
         (define full-args (append (map car pargs) nargs))
         (define gens
           (list
            (cons #`gen:term
                  (with-syntax ([(args ...) full-args])
                    #`((define (gmap-t ff f v)
                         (match v [(#,nid args ...)
                                   ((ff v) (f args) ...)])))))))
         (for/fold ([res `()])
                   ([g gens])
           `(,@res ,#`#:methods ,(car g) ,(cdr g))))]))

(define-syntax (map-generic ast-spec)
  #f)

(define-syntax (sexp-printer ast-spec)
  #f)
