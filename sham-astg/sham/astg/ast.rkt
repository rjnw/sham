#lang racket

(require
 (for-syntax
  "private/ast-syntax-structs.rkt"
  "private/ast-syntax-class.rkt"
  syntax/parse
  racket/syntax
  racket/pretty))

(provide define-ast)

(begin-for-syntax
  (require racket)
  (require syntax/datum)
  (define (node-args node-pat)
    (define (rec pat)
      (match pat
        [(ast:pat:single s)  s]
        [(ast:pat:multiple m) (map rec m)]
        [(ast:pat:repeat r) (rec r)]
        [(ast:pat:checker c s) s]
        [(ast:pat:datum _) '()]))
    (rec node-pat))
  (define (flatten-node-args node-pat)
    (flatten (node-args node-pat)))
  (define (shorten-node-arg arg-id)
    (node-arg-id arg-id))

  ;; id:terminal.sym -> (values "id" (list "terminal" "sym")
  (define (split-node-arg i)
    (define id-gn (string-split (symbol->string (syntax->datum i)) ":"))
    (define group-node (if (>= (length id-gn) 2) (string-split (second id-gn) ".") #f))
    (cons (first id-gn) group-node))
  (define (simple-node-arg? s)
    (define splits (split-node-arg s))
    (false? (cdr splits)))
  (define (node-arg-id s)
    (define splits (split-node-arg s))
    (datum->syntax s (string->symbol (car splits))))
  (define (node-arg-group s)
    (define splits (split-node-arg s))
    (unless (cdr splits)
      (error "node doesn't specify ast group" s))
    (datum->syntax s (string->symbol (second splits))))

  (define (info-args meta-info)
    (match meta-info
      ['() '()]
      [`(((common) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
      [`(((common auto) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
      [`(((common mutable) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
      [`(((common auto mutable) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
      [else (info-args (cdr meta-info))]))

  (define (meta-args meta-info
                     #:common (cc identity)
                     #:common-mutable (cm identity)
                     #:common-auto (ca identity)
                     #:common-auto-mutable (cam identity))
    (define (rec mi)
      (match mi
        ['() '()]
        [`(((common) . ,c) . ,rst) (cons (cc c) (rec rst))]
        [`(((common mutable) . ,c) . ,rst) (cons (cm c) (rec rst))]
        [`(((common auto) . ,c) . ,rst) (cons (ca c) (rec rst))]
        [`(((common auto mutable) . ,c) . ,rst) (cons (cam c) (rec rst))]
        [else (meta-args (cdr meta-info))]))
    (rec meta-info))

  (define (map-pat pat f-single f-datum f-multiple f-repeat)
    (define (rec pat)
      (match pat
        [(ast:pat:single s) (f-single s)]
        [(ast:pat:datum d) (f-datum d)]
        [(ast:pat:checker c s) (f-single s)]
        [(ast:pat:multiple s) (f-multiple (map rec s))]
        [(ast:pat:repeat r) (f-repeat (rec r))]))
    (rec pat))

  (define (node-display var node-pat)
    (define (rec pat)
      (match pat
        [(ast:pat:single s) #`,#,(shorten-node-arg s)]
        [(ast:pat:datum d) d]
        [(ast:pat:multiple s) #`(#,@(map rec s))]
        [(ast:pat:checker c s) #`#,(shorten-node-arg s)]
        [(ast:pat:repeat r) (if (ast:pat:single? r)
                                #`,@`#,(rec r)
                                #`,@(apply (curry map list) `#,(rec r)))]))
    (define p (rec node-pat))
    (if (ast:pat:multiple? node-pat)
        #``(#,var ,@`#,p)
        #``#,p))

  (define (build-defs top meta-spec)
    (match-define (cons meta spec) meta-spec)
    (define (get-meta sym default)
      (if (assoc sym meta) (cdr (assoc sym meta)) default))
    (define prefix (get-meta 'prefix top))
    (define seperator (get-meta 'seperator #': ))
    (define top-seperator (get-meta 'top-seperator #':))
    (define build-map? (get-meta 'build-map #f))
    (define build-macros? (get-meta 'build-macros #f))
    (define custom-write? (get-meta 'custom-write #f))

    (define (build-group-map spec)
      (match spec
        [`(,groups ...)
         (map build-group-map groups)]
        [(ast:group name _ _ meta-info)
         (cons (syntax->datum name) spec)]))
    (define group-map (make-hash (build-group-map spec)))

    (define (get-group-spec group-id)
      (if group-id (hash-ref group-map (syntax->datum group-id)) #f))
    (define group-name ast:group-name)
    (define (group-id spec)
      (match spec
        [#f prefix]
        [(ast:group name parent _ _)
         (if (get-group-spec parent)
             (format-id top "~a~a~a" (group-id (get-group-spec parent)) seperator name)
             (if prefix
                 (format-id top "~a~a~a" prefix top-seperator name)
                 name))]))
    (define (node-id node-spec group-spec)
      (define var
        (match node-spec
          [(ast:node var _ _) var]
          [(ast:term-node var _) var]))
      (format-id var "~a~a~a" (group-id group-spec) seperator var))
    (define (group-args group-spec)
      (match-define (ast:group id parent node meta-info) group-spec)
      (append (if parent (group-args (hash-ref group-map (syntax->datum parent))) empty)
              (info-args meta-info)))
    (define (group-terminals meta-info)
      (match meta-info
        [`() #f]
        [`(((terminals) . ,terminals) . ,rst) terminals]
        [else (group-terminals (cdr meta-info))]))
    (define (group-def group-spec)
      (match-define (ast:group name parent node-specs meta-info) group-spec)
      ;; (printf "\n\ngroup: ~a\n" (syntax->datum name))

      (define args (meta-args meta-info
                              #:common-auto (λ (v) #`(#,v #:auto))
                              #:common-mutable (λ (v) #`(#,v #:mutable))
                              #:common-auto-mutable (λ (v) #`(#,v #:auto #:mutable))))
      (define parent-args (group-args group-spec))
      (define pargs (map cdr parent-args))
      (define (group-generic-id stx) (format-id stx "~ag" stx))
      (define (group-generic-map stx) (format-id stx "map-~a" stx))
      (define (group-generic-function stx) (format-id stx "f-~a" stx))
      (define gid (group-id group-spec))
      (define gname (group-name group-spec))
      (define generic-id (group-generic-id gname))
      (define generic-map-id (group-generic-map gname))
      (define generic-map-fs (map (compose group-generic-function group-name) spec))

      (define (node-def node-spec)
        (match-define (ast:node var pat meta-info) node-spec)
        (define (mutable-arg? a)
          (for/or ([mi meta-info])
            (match mi
              [`(mutable . ,arg)
               (equal? (syntax->datum a) (syntax->datum arg))]
              [else false])))
        (define meta-extras
          (flatten
           (for/list ([mi meta-info])
             (match mi
               [`(extra . ,v) (syntax->list v)]
               [else '()]))))
        (define id (node-id node-spec group-spec))
        (define args (node-args pat))
        (define short-flat-args (map shorten-node-arg (flatten args)))
        (define constructor-args (map (λ (a) (if (mutable-arg? a) #`(#,a #:mutable) a)) short-flat-args))
        (define full-args (append pargs short-flat-args))
        (define methods
          (append
           meta-extras
           (if custom-write?
               (list #`#:methods #`gen:custom-write
                     #`((define (write-proc struc port mode)
                          (match-define (#,id #,@full-args) struc)
                          (display #,(node-display var pat) port)
                          #;(match mode
                              [#t (write `(#,id #,@full-args) port)]
                              [#f (display #,(node-display var pat) port)]
                              [else (print `(#,id #,@full-args) port)])
                          )))
               '())
           (if build-map?
               (list
                #`#:methods (format-id generic-id "gen:~a" generic-id)
                #`((define (#,generic-map-id  #,@generic-map-fs #,generic-id)
                     (match-define (#,id #,@full-args) #,generic-id)
                     ;; todo: fix for common auto mutable args
                     (#,id #,@pargs
                      #,@(map-pat pat
                                  (λ (s) (if (simple-node-arg? s)
                                             (list s)
                                             (list #`(#,(group-generic-function (node-arg-group s))
                                                      #,(shorten-node-arg s)))))
                                  (const '())
                                  (λ (m) (flatten m))
                                  (λ (r) (map (λ (r) (if (syntax->list r) #`(map #,@r) (list r))) r)))))))
               '())))
        #`(struct #,id #,(group-id group-spec) #,constructor-args #,@methods))
      (define (term-node-def node-spec)
        (match-define (ast:term-node var proc) node-spec)
        (define id (node-id node-spec group-spec))
        (define methods
          (append
           (if custom-write?
               (list #`#:methods #`gen:custom-write
                     #`((define (write-proc struc port mode)
                          (match-define (#,id val) struc)
                          (display val ;; `(#,(shorten-node-arg var) ,val)
                                   port))))
               '())
           (if build-map?
               (list #`#:methods (format-id generic-id "gen:~a" generic-id)
                     #`((define (#,generic-map-id  #,@generic-map-fs #,generic-id)
                          (match-define (#,id #,@pargs val) #,generic-id)
                          (#,id #,@pargs val ;; (#,(group-generic-function name) val) TODO
                           ))))
               '())))
        #`(struct #,id #,(group-id group-spec) (#,(shorten-node-arg var)) #,@methods))
      (define (general-node-def node-spec)
        (match node-spec
          [(ast:node _ _ _) (node-def node-spec)]
          [(ast:term-node _ _) (term-node-def node-spec)]))
      (define node-defs (flatten (map general-node-def node-specs)))
      (append
       (list
        (if parent
            #`(struct #,gid #,(group-id (get-group-spec parent)) (#,@args))
            #`(struct #,gid (#,@args))))
       (if build-map?
           (list #`(define-generics #,generic-id
                     (#,generic-map-id #,@generic-map-fs #,generic-id)))
           '())
       node-defs))
    (define group-defs (flatten (map group-def spec)))
    group-defs)

  (define (spec->storage top ast-spec)
    (define (group-storage spec)
      (define (node-storage spec)
        (define (pattern-storage pat)
          (match pat
            [(ast:pat:single s) #`(list 'single #'#,s)]
            [(ast:pat:datum s) #`(list 'datum `#,s)]
            [(ast:pat:multiple s) #`(list 'multiple #,@(map pattern-storage s))]
            [(ast:pat:checker c s) #`(list 'single #'#,s)]
            [(ast:pat:repeat s) #`(list 'repeat #,(pattern-storage s))]))
        (match spec
          [(ast:node variable pattern meta-info) #`(list 'ast:node #'#,variable #,(pattern-storage pattern) '#,meta-info)]
          [(ast:term-node variable proc) `(list 'ast:term-node #'#,variable #'#,proc)]))
      (match-define (ast:group name parent nodes meta-info) spec)
      #`(list 'ast:group #'#,name #'#,parent (list #,@(map node-storage nodes)) '#,meta-info))
    ;; todo add meta info
    #`(list #'#,top (list #,@(map group-storage (cdr ast-spec)))))

  (define (storage->spec storage)
    (define (group-spec storage)
      (define (node-spec storage)
        (define (pattern-spec storage)
          (match storage
            [`(single ,s) (ast:pat:single s)]
            [`(datum ,s) (ast:pat:datum s)]
            [`(multiple ,s ...) (ast:pat:multiple (map pattern-spec s))]
            [`(repeat ,s) (ast:pat:repeat (pattern-spec s))]))
        (match-define `(ast:node ,variable ,pat ,meta-info) storage)
        (ast:node variable (pattern-spec pat) meta-info))
      (match-define `(ast:group ,name ,parent ,nodes ,meta-info) storage)
      (ast:group name parent (map node-spec nodes) meta-info))
    (match-define `(,top ,groups) storage)
    (values top (map group-spec groups))))

;; TODO
;; * get parents of super group
;; * use the common attributes in writer pattern
;; * figure out the reader format with the node names

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define ast-spec (attribute gs.spec))
     (define struct-defs (build-defs #'cid ast-spec))
     ;; (pretty-display ast-spec)
     ;; (printf "struct-defs:\n")
     ;; (parameterize ([pretty-print-columns 80])
     ;;   (pretty-print (map syntax->datum struct-defs)))
     #`(begin
         (require racket/generic)
         (define cid #,(spec->storage #'cid ast-spec))
         #,@struct-defs)]))

;; (module+ test
;;   (define-ast LC
;;     #:prefix ||
;;     #:top-seperator ||
;;     #:seperator -
;;     (expr
;;      [lambda ((n:terminal.sym) body:expr)]
;;      [letrec (((ids:terminal.sym vals:expr) ...) e:expr)]
;;      [app (rator:expr rand:expr)]
;;      [sym s:terminal.sym])
;;     (terminal #:terminals
;;               [n number?]
;;               [sym symbol?]))
;;   ;; (define lr (LC:expr:letrec '(a b c) '(1 2 3) 'd))
;;   ;; (printf "LC:")
;;   ;; (pretty-print LC)
;;   )
