#lang racket

(require
 (for-syntax "generics.rkt"
             "syntax/spec.rkt"
             syntax/parse
             racket/match
             racket/syntax
             racket/pretty)
 racket/generic
 "runtime.rkt")

(provide (all-defined-out))

(begin-for-syntax
  ;; (define (ast-struct-rkt:add-generic-method asr gen-id gen-methods)
  ;;   (update-ast-struct-rkt-option asr methods (λ (ms) (append ms (list gen-id gen-methods))) '()))

  ;; (struct ast-generic-map-builder []
  ;;   #:methods gen:ast-builder
  ;;   [(define (build-top-struct ab tstruct as) tstruct)
  ;;    (define (build-group-struct ab gstruct as gs) gstruct)
  ;;    (define (build-group-extra ab gextra as gs) gextra)
  ;;    (define (build-node-struct ab nstruct as gs ns)
  ;;      (let* ([gen-id #`gen:gterm]
  ;;             [gen-syntax
  ;;              #`((define/generic super-gmap-t gmap-t)
  ;;                 (define (gmap-t ff f v)
  ;;                   (match-define (ast:term md args vals) v)
  ;;                   ((ff v) md (super-gmap-t ff f args) (super-gmap-t ff f vals))
  ;;                   #;(match v [(#,nid md args ...) ((ff v) md (f args) ...)])))])
  ;;        (ast-struct-rkt:add-generic-method nstruct gen-id gen-syntax)))
  ;;    (define (build-node-extra ab nextra as gs ns) nextra)])
  ;; (define ast-gmap-builder (ast-generic-map-builder))

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
  ;; (letrec ([ids vals] ...) body) -> (vector (list (vector id val)) body) -> `(letrec (vector-ref args 0) ,@(map (lambda (v) (list (vector-ref v 0) (vector-ref v 1))) (vector-ref args 1)))
  (define (build-printer fp pat args)
    (let rec ([p pat]
              [from args])
      (match p
        [(ast:pat:single chk i) #`(#,fp #,from)]
        [(ast:pat:datum d) #`'#,d]
        [(ast:pat:multiple ps)
         (define (datums-until to (ds 0) (ci 0))
           (cond [(>= ci to) ds]
                 [(ast:pat:datum? (vector-ref ps ci)) (datums-until to (add1 ds) (add1 ci))]
                 [else (datums-until to ds (add1 ci))]))
         (define (fm pp i)
           (cond
             [(ast:pat:datum? pp) (rec pp #f)]
             [else (rec pp #`(vector-ref #,from #,(- i (datums-until i))))]))
         #`(list #,@(for/list ([p (in-vector ps)] [i (vector-length ps)]) (fm p i)))]
        [(ast:pat:repeat pp k)
         (define v (generate-temporary 'v))
         #`(map (lambda (#,v) #,(rec pp v)) #,from)])))

  (define-ast-builder (sexp-printer)
    (build-group
     (gcs as gs)
     (match-define (ast tid (ast:id tid-o tid-g tid-f) groups info) as)
     (match-define (ast:group (ast:id gid-o gid-t gsyn-id) parent gargs nodes ginfo) gs)
     (with-syntax ([pprint-id (format-id gid-o "pprint-~a" gid-o)])
       (cons #`(define (pprint-id term) #`(ast:group-args term)) gcs)))
    (build-node
     (ncs as gs ns)
     (match-define (ast tid (ast:id tid-o tid-g tid-f) groups info) as)
     (match-define (ast:group (ast:id gid-o gid-t gsyn-id) parent gargs nodes ginfo) gs)
     (match-define (ast:node (ast:id nid-o nid-t nsyn-id) nargs pat ninfo) ns)
     (with-syntax ([pprint-id (format-id nid-o "pprint-~a" nid-o)]
                   [pprec (generate-temporary #'pp)])
       (cons #`(define (pprint-id term pprec) #,(build-printer #`pprec pat #`(ast:term-args term)))
             ncs)))))

;; (define-syntax (map-generic ast-spec)
;;   ast-gmap-builder)

(define-syntax (sexp-printer)
  (sexp-printer-builder))

(define-syntax (struct-helpers)
  (rkt-struct-functions-builder))
