#lang racket

(require
 (for-syntax "generics.rkt"
             "spec.rkt"
             syntax/parse
             racket/syntax
             racket/pretty)
 racket/generic
 "runtime.rkt")

(provide map-generic sexp-printer)

(begin-for-syntax
  (define (ast-struct-rkt:add-generic-method asr gen-id gen-methods)
    (update-ast-struct-rkt-option asr methods (λ (ms) (append ms (list gen-id gen-methods))) '()))

  (struct ast-generic-map-builder []
    #:methods gen:ast-builder
    [(define (build-top-struct ab tstruct as) tstruct)
     (define (build-group-struct ab gstruct as gs) gstruct)
     (define (build-group-extra ab gextra as gs) gextra)
     (define (build-node-struct ab nstruct as gs ns)
       (let* ([gen-id #`gen:gterm]
              [nid (ast:basic-syn-id ns)]
              [full-args (map ast:basic-syn-id
                              (append (ast:group-args gs)
                                      (ast:node-args ns)))]
              [gen-syntax
               #`((define/generic super-gmap-t gmap-t)
                  (define (gmap-t ff f v)
                    (match-define (ast:term md args vals) v)
                    ((ff v) md (super-gmap-t ff f args) (super-gmap-t ff f vals))
                    #;(match v [(#,nid md args ...) ((ff v) md (f args) ...)])))])
         (ast-struct-rkt:add-generic-method nstruct gen-id gen-syntax)))
     (define (build-node-extra ab nextra as gs ns) nextra)])
  (define ast-gmap-builder (ast-generic-map-builder))

  ;; (struct ast-map-builder []
  ;;   #:methods gen:ast-builder
  ;;   [(define (build-top-struct ab tstruct as) tstruct)
  ;;    (define (build-group-struct ab gstruct as gs) gstruct)
  ;;    (define (build-group-extra ab gextra as gs)
  ;;      gextra)


  ;;    (define (build-node-struct ab nstruct as gs ns)
  ;;      (let* ([gen-id #`gen:term]
  ;;             [nid (ast:basic-syn-id ns)]
  ;;             [full-args (map ast:basic-syn-id
  ;;                             (append (ast:group-args gs)
  ;;                                     (ast:node-args ns)))]
  ;;             [gen-syntax
  ;;              (with-syntax ([(args ...) full-args])
  ;;                #`(...))])
  ;;        (ast-struct-rkt:add-generic-method nstruct gen-id gen-syntax)))
  ;;    (define (build-node-extra ab nextra as gs ns) nextra)]
  ;;   [(define (build-top-struct)
  ;;      #f)
  ;;    (define (build-group-generics ab fmt gs)
  ;;      (define ast-spec (ast-map-generic-ast-spec ab))
  ;;      (define gid (format-group-id fmt gs))
  ;;      (define generic-id (format-id #f "mapg-~a" gid))
  ;;      (define generic-map-id (format-id #f "map-~a" gid))
  ;;      (define generic-map-fs
  ;;        (map (λ (g) (format-id #f "f-~a" (format-group-id fmt g)))
  ;;             (cond [(ast-groups ast-spec)]
  ;;                   [else (hash-values (ast-groups ast-spec))])))
  ;;      (list #`(define-generics #,generic-id
  ;;                (#,generic-map-id #,@generic-map-fs #,generic-id))))
  ;;    (define (build-node-methods ab fmt gs ns)
  ;;      (define ast-spec (ast-map-generic-ast-spec ab))
  ;;      (define gid (format-group-id fmt gs))
  ;;      (define nid (format-node-id fmt gs ns))
  ;;      (define generic-id (format-id #f "mapg-~a" gid))
  ;;      (define generic-map-id (format-id #f "map-~a" gid))
  ;;      (define generic-map-fs
  ;;        (map (λ (g) (format-id #f "f-~a" (format-group-id fmt g)))
  ;;             (cond [(ast-groups ast-spec)]
  ;;                   [else (hash-values (ast-groups ast-spec))])))
  ;;      (define pargs (group-args ast-spec gs))
  ;;      (define nargs (node-args ns))
  ;;      (define full-args (append (map car pargs) nargs))
  ;;      (define gens
  ;;        (list
  ;;         (cons #`gen:term
  ;;               (with-syntax ([(args ...) full-args])
  ;;                 #`((define (gmap-t ff f v)
  ;;                      (match v [(#,nid args ...)
  ;;                                ((ff v) (f args) ...)])))))))
  ;;      (for/fold ([res `()])
  ;;                ([g gens])
  ;;        `(,@res ,#`#:methods ,(car g) ,(cdr g))))])
  )

(define-syntax (map-generic ast-spec)
  ast-gmap-builder)

(define-syntax (sexp-printer ast-spec)
  #f)
