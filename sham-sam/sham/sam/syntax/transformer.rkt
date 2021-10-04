#lang racket

(require syntax/parse
         racket/syntax
         racket/exn
         (for-template racket racket/match))

(require (prefix-in rt: (submod "runtime.rkt" ast))
         (submod "spec.rkt" ast)
         "pat.rkt"
         "kw-info.rkt"
         "utils.rkt"
         (for-template (prefix-in rrt: "../runtime.rkt")))

(provide rkt-pattern-transformer
         rkt-match-expander)

#;(- there are three different options for providing macros for production nodes
     * generate a syntax macro for each production
     * write a conventional pattern matcher that performs
     * make use of racket's pattern matcher and convert our patterns to racket based on the storage format

     We use the third option here using fold-with-pattern which takes a pattern and syntax folds
     over the syntax according to the pattern)

(define (stx-with-pat->match-pattern stx pat (ignore-dat? #t))
  (define norm-stx
    (match pat
      [(pat:var s)
       (if (equal? (length (syntax-e stx)) 1)
           (car (syntax-e stx))
           (error 'sham/sam/internal "cannot match single pattern with a list syntax ~a ~a\n" pat stx))]
      [(pat:dat d)
       (cond
         [(equal? (length (syntax-e stx)) 1) (car (syntax-e stx))]
         ;; [(empty? (syntax-e stx)) stx]
         [else stx])]
      [else stx]))
  (define (rec ps)
    (match ps
      [`(seq ,args ,pat) #`(vector #,@(flatten (map rec (reverse args))))]
      [`(ooo ,args ,pat) #`(list #,@(flatten (map rec (reverse args))))]
      [`(var ,arg ,pat) #`#,arg]
      [`(dat ,s ,pat) (if s #`'#,s '())]
      [`(.. ,s ,pat) #`#,s]
      [else (error 'sham/sam/internal "could not match parsed syntax ~a in pattern ~a\n" ps pat)]))
  (define fdat (cond [(procedure? ignore-dat?) ignore-dat?]
                     [else (if ignore-dat? ignore-dodat default-dodat)]))
  (if (pat:dat? pat)
      #`(vector)
      (match (parse-stx-with-pattern pat norm-stx fdat)
        [(stk '() (list parsed)) (rec parsed)]
        [err (error 'sham/sam/pat "cannot parse ~a with ~a, err: ~a" stx pat err)])))

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
  (call-with-exception-handler handle-exn (thunk (stx-with-pat->match-pattern stx pat))))

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
        #`(#,(get-struct-id ids)
           (rrt:generic-metadata
            (~? md #,(default-metadata ninfo ginfo tinfo))
            #:srcloc (rrt:ast:location #,(syntax-srcloc stx) '#%from-pattern-constructor))
           (vector #,@gargs-stx) #,nargs-stx)]
       [(ast:group ids ginfo prnt gargs nodes)
        (define gargs (full-group-args ss ts))
        (define-values (gargs-stx rest-stx) (match-group-args gargs (syntax-e #`(args ...))))
        #`(#,(get-struct-id ids)
           (rrt:generic-metadata #:tag '#%from-pattern-constructor (~? md #,(default-metadata ginfo tinfo)))
           (vector #,@gargs-stx) (vector))])]))
