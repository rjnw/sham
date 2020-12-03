#lang racket

(require
 (for-syntax
  "private/ast-syntax-structs.rkt"
  "private/ast-syntax-class.rkt"
  "private/ast-syntax-generics.rkt"
  syntax/parse
  racket/syntax
  racket/pretty)
 racket/generic)

(provide (all-defined-out))

(begin-for-syntax
  (struct ast-map-generic [ast-id ast-spec]
    #:methods gen:ast-builder
    [(define (build-group-methods ab fmt gs)
       #f)
     (define (build-group-generics ab fmt gs)
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
       (define gid (format-group-id fmt gs))
       (define nid (format-node-id fmt gs ns))
       (define generic-id (format-id #f "mapg-~a" gid))
       (define generic-map-id (format-id #f "map-~a" gid))
       (define generic-map-fs
         (map (λ (g) (format-id #f "f-~a" (format-group-id fmt g)))
              (cond [(ast-groups ast-spec)]
                    [else (hash-values (ast-groups ast-spec))])))
       (define (group-args gs)
         (if gs
             (append (map (λ (s)
                            (syntax-parse s
                              [i:identifier (cons #`i #f)]
                              [(i:identifier ki:keyword-info) (cons #`i (attribute ki.spec))]))
                          (info-values ginfo `#:common))
                     (group-args (lookup-group-spec ast-spec (ast:group-parent gs))))
             `()))
       (define pargs (group-args gs))
       (define nargs (node-args ns))
       (define full-args (append (map car pargs) nargs))
       (list #`#:methods #,generic-id
             #`((define (#,generic-map-id #,@generic-map-fs #,generic-id)
                  (match-define (#,id #,@full-args) #,generic-id)
                  (#,node-id #,@pargs
                   #,@nargs
                   ;; #,@(map (λ (arg)
                   ;;           (match (node-arg-contract arg)
                   ;;             [`(group=? ,grp) ...]
                   ;;             [`any/c arg]
                   ;;             [`(repeat? ,x) (map arg)])) nargs)
                   ;; #,@(map-pat pat
                   ;;             (λ (s) (if (simple-node-arg? s)
                   ;;                        (list s)
                   ;;                        (list #`(#,(group-generic-function (node-arg-group s))
                   ;;                                 #,(shorten-node-arg s)))))
                   ;;             (const '())
                   ;;             (λ (m) (flatten m))
                   ;;             (λ (r) (map (λ (r) (if (syntax->list r) #`(map #,@r) (list r))) r)))
                   )))))]))

(define-syntax (map-generic ast-id ast-spec)
  (printf "hello from map-generic\n")
  #f)

(define-syntax (sexp-printer ast-id ast-spec)
  (printf "hello from sexp-printer\n")
  #f)
