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

(define-generics term
  ;; gmap-t : (-> a (-> b ... c)) (-> x b) a)
  ;; ((ff term) ((f <term-internal-args>) ...))
  (gmap-t ff f term)
  #:defaults ([list?
               (define (gmap-t ff f term)
                 (apply (ff term) (map f term)))]))

(begin-for-syntax
  (struct ast-map-generic [ast-id ast-spec]
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
       (define (group-args gs)
         (if gs
             (append (map (λ (s)
                            (syntax-parse s
                              [i:identifier (cons #`i #f)]
                              [(i:identifier ki:keyword-info) (cons #`i (attribute ki.spec))]))
                          (info-values (ast:group-info gs) `#:common))
                     (group-args (lookup-group-spec ast-spec (ast:group-parent gs))))
             `()))
       (define pargs (group-args gs))
       (define nargs (node-args ns))
       (define full-args (append (map car pargs) nargs))
       (define gens
         (list
          ;; (cons generic-id
          ;;       #`((define (#,generic-map-id #,@generic-map-fs #,generic-id)
          ;;            (match-define (#,nid #,@full-args) #,generic-id)
          ;;            (#,nid #,@pargs
          ;;             #,@nargs
          ;;             ;; #,@(map (λ (arg)
          ;;             ;;           (match (node-arg-contract arg)
          ;;             ;;             [`(group=? ,grp) ...]
          ;;             ;;             [`any/c arg]
          ;;             ;;             [`(repeat? ,x) (map arg)])) nargs)
          ;;             ;; #,@(map-pat pat
          ;;             ;;             (λ (s) (if (simple-node-arg? s)
          ;;             ;;                        (list s)
          ;;             ;;                        (list #`(#,(group-generic-function (node-arg-group s))
          ;;             ;;                                 #,(shorten-node-arg s)))))
          ;;             ;;             (const '())
          ;;             ;;             (λ (m) (flatten m))
          ;;             ;;             (λ (r) (map (λ (r) (if (syntax->list r) #`(map #,@r) (list r))) r)))
          ;;             ))))
          (cons #`gen:term
                (with-syntax ([(args ...) full-args])
                  #`((define (gmap-t ff f v)
                       (match v [(#,nid args ...)
                                 ((ff v) (f args) ...)])))))))
       (for/fold ([res `()])
                 ([g gens])
         `(,@res ,#`#:methods ,(car g) ,(cdr g))))]))

(define-syntax (map-generic ast-id ast-spec)
  #f)

(define-syntax (sexp-printer ast-id ast-spec)
  #f)
