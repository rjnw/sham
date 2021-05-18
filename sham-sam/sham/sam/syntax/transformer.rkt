#lang racket

(require syntax/parse
         racket/syntax
         racket/exn
         (for-template racket racket/match))

(require (prefix-in rt: "runtime.rkt")
         "spec.rkt"
         "pattern.rkt"
         "private/utils.rkt"
         (for-template (prefix-in rrt: "../runtime.rkt")))

(provide rkt-pattern-transformer
         rkt-match-expander)

#;(- there are three different options for providing macros for production nodes
     * generate a syntax macro for each production
     * write a conventional pattern matcher that performs
     * make use of racket's pattern matcher and convert our patterns to racket based on the storage format

     We use the third option here using fold-with-pattern which takes a pattern and syntax folds
     over the syntax according to the pattern)

(define (match-group-args gargs stxs)
  (match* (gargs stxs)
    [('() ss) (values '() ss)]
    [((cons gs grs) (cons ss srs))
     (define-values (gr sr) (match-group-args grs srs))
     (values (cons ss gr) sr)]
    [(_ _) (error 'sham/sam "not enough arguments for common group args")]))
(define (match-node-args node-spec stx orig-stx)
  (match-define (ast:node ids ninfo nargs pat) node-spec)
  (define (handle-exn ex)
    (raise (error 'sham/sam "error matching arguments with grammar:\n~a\n~a\n" (get-oid ids) orig-stx)))
  (call-with-exception-handler handle-exn (thunk (expand-with-pattern pat stx))))
(define (rkt-match-expander tt stx)
  (match-define (rt:term-type rt mt ss ts) tt)
  (syntax-parse stx
    [(_ (~optional (~seq (~datum #:md) md)) args ...)
     (match ss
       [(ast:node ids ninfo nargs pat)
        (define gs (find-node-group ss ts))
        (define gargs (full-group-args gs ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        (define nargs-stx (match-node-args ss #`(#,@rest-stx) stx))
        #`(#,(get-struct-id ids) (~? md _) (vector #,@gargs-stx) #,nargs-stx)]
       [(ast:group ids ginfo prnt gargs nodes)
        (define gargs (full-group-args ss ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        ;; (error 'sham:sam "todo match expander for group types: ~a" (ast:id-orig ids))
        #`(#,(get-struct-id ids) (~? md _) (vector #,@gargs-stx) _)])]))

(define (rkt-pattern-transformer tt stx)
  (match-define (rt:term-type rt mt ss ts) tt)
  (match-define (ast tid tids grps tinfo) ts)
  (define default-tmd (info-1value 'default-metadata tinfo))
  (syntax-parse stx
    [nid:id (get-struct-id (ast:basic-id ss))]
    [(_ (~optional (~seq (~datum #:md) md:expr)) args ...)
     (match ss
       [(ast:node ids ninfo nargs pat)
        (define gs (find-node-group ss ts))
        (match-define (ast:group gids ginfo gprnt giargs gnodes) gs)
        (define gargs (full-group-args gs ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        (define nargs-stx (match-node-args ss #`(#,@rest-stx) stx))
        (define default-nmd (info-1value 'default-metadata ninfo))
        (define default-gmd (info-1value 'default-metadata ginfo))
        #`(#,(get-struct-id ids)
           (rrt:generic-metadata
            (~? md #,(or default-nmd default-gmd default-tmd))
            #:srcloc (rrt:ast:location #,(syntax-srcloc stx) '#%from-pattern-constructor))
           (vector #,@gargs-stx) #,nargs-stx)]
       [(ast:group ids ginfo prnt gargs nodes)
        (define gargs (full-group-args ss ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        (define default-gmd (info-1value 'default-metadata ginfo))
        #`(#,(get-struct-id ids)
           (rrt:generic-metadata #:tag '#%from-pattern-constructor (~? md #,(or default-gmd default-tmd)))
           (vector #,@gargs-stx) (vector))])]))
