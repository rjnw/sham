#lang racket

(require racket/syntax
         syntax/parse
         (for-template racket/base))

(require "spec.rkt"
         (submod "private/spec.rkt" reader)
         "pattern.rkt")

(provide (all-defined-out))

(define (build-parsers rspec)
  (match-define (reader ast-id info) rspec)
  (define-values (ast-spec _) (syntax-local-value/immediate ast-id))
  (match-define (ast rid ids groups ainfo) ast-spec)
  (define oid (get-oid ids))
  (define (parser-id id (ctxt id)) (format-id ctxt "parse-syntax-~a" id))
  (define (find-spec path (ctx ast-spec))
    (define (next sid)
      (match ctx
        [(ast rid ids groups ainfo)
         (cond
           [(find-group-spec sid ctx)]
           [else (find-group/node-spec sid ctx)])]
        [(ast:group ids ginfo gparent gargs gnodes) (find-node-spec sid ctx ast-spec)]
        [else (error 'sham/sam "next element in path not found: ~a ~a ~a" sid path ctx)]))
    (match path
      [(list t) (next t)]
      [(cons a d) (find-spec d (next a))]))
  (define (orig-full-id path) (get-fid (ast:basic-id (find-spec path))))
  (define (parser-from-path path)
    (printf "pfp: ~a\n" path)
    (parser-id (orig-full-id path) (car path)))
  (define (group-parser group-spec-pair)
    (match-define (cons gsyn-id (ast:group gids ginfo gparent gargs gnodes)) group-spec-pair)
    (printf "building-group-parser: ~a\n" gsyn-id)
    (define (node-parser node-spec-pair)
      (match-define (cons nsyn-id (ast:node nids ninfo nargs npat)) node-spec-pair)
      (printf "building-node-parser: ~a\n" nsyn-id)
      (define nid (get-oid nids))
      (define (pattern->value pat)
        (define (parse-type of)
          (match of
            ['() of]
            [(list grp subs ...) #`(#,(parser-from-path (cons grp subs)) #,of)]))
        (match pat
          [(ast:pat:single check id)
           (define arg (find-node-arg (cdr node-spec-pair) id))
           (unless arg (error 'sham/sam "node argument not found ~a ~a.~a" ast-id nid id))
           (match-define (ast:node:arg aids ainfo atype) arg)
           (match atype
             [(ast:type:internal depth of) #`(#,(parse-type of) #,id)]
             [else
              (cond
                [check #`(if (#,check #,id) #,id (error 'check-failed))]
                [else id])])]
          [(ast:pat:datum syn) #f]
          [(ast:pat:multiple ps) #`(vector #,@(filter-map pattern->value (vector->list ps)))]
          [(ast:pat:repeat p cnt) (pattern->value p)]))
      (define (pattern->syntax pat)
        (define (ppat p)
          (match p
            [(ast:pat:single check id) id]
            [(ast:pat:datum syn) #`(~datum #,syn)]
            [(ast:pat:multiple ps) #`(#,@(flatten (for/list ([p ps]) (ppat p))))]
            [(ast:pat:repeat p cnt)
             (match cnt
               [(cons mn mx) #:when (and mn mx) #`(~seq (~between #,(ppat p) #,mn #,mx))]
               [else (list (ppat p) #`(... ...))])]))
        (ppat pat))
      (define (pattern->value+syntax pat)
        (struct ctxt [val syn] #:prefab)
        (define (parse-from-type atype vid)
          (match atype
            [(ast:type:internal depth of)
             (with-syntax ([parser-op (parser-from-path of)]
                           [value-syn vid])
               #`(parser-op value-syn))]
            [(ast:type:check depth chk)
             #`(if (#,chk #,vid)
                   #,vid
                   (error 'sham/sam "ast specified type check failed ~a ~a"
                          '#,(get-oid nids) '(#,chk #,vid)))]
            [else vid]))
        (define (maybe-syntax-class atype vid) ;; TODO add syntax class for intrinsic and identifier types
          vid)
        (define (f c pat path)
          (printf "zipper:f: \n\t~a\n\t~a\n\t~a\n" c pat path)
          (match-define (ctxt val syn) c)
          (match pat
            [(ast:pat:single check id)
             (define arg (find-node-arg (cdr node-spec-pair) id))
             (unless arg (error 'sham/sam "node argument not found ~a.~a" node-spec-pair id))
             (match-define (ast:node:arg aids ainfo atype) arg)
             (ctxt (cons (parse-from-type atype (get-fid aids)) val)
                   (cons (maybe-syntax-class atype (get-fid aids)) syn))]
            [(ast:pat:datum d)
             (ctxt val
                   (cons #`(~datum #,d) syn))]
            [(ast:pat:multiple ps)
             (match-define `(at-multiple ,frec ,ppath) path)
             (match-define (ctxt mlt-val mlt-syn)
               (for/fold ([c (ctxt '() '())])
                         ([p ps]
                          [i (vector-length ps)])
                 (frec c i)))
             (ctxt (cons #`(vector #,@(reverse mlt-val)) val)
                   (cons (datum->syntax #f (reverse mlt-syn)) syn))]
            [(ast:pat:repeat p (cons mn mx))
             (match-define `(at-repeat ,frec ,ppath) path)
             (match-define (ctxt rval rsyn) (frec (ctxt '() '()) (cons mn mx)))
             (ctxt (list* #`(... ...) (car rval) val)
                   (cond
                     [(number? mn) (cons #`(~seq (~between #,(car rsyn) #,mn #,(or mx +inf.0))) syn)]
                     [else (list* #`(... ...) (car rsyn) syn)]))]))
        (match-define (ctxt val syn) (pattern-with-zipper f (ctxt '() '()) pat))
        (match pat
          [(ast:pat:datum d) (values #`(vector) (car syn))]
          [else (values (car val) (car syn))]))
      (define-values (pv ps) (pattern->value+syntax npat))
      ;; (printf "v+s: \n") (pretty-print (cons pv ps)) (newline)
      (let* ([fid (get-fid nids)]
             [parser-name (parser-id fid)])
        (cons parser-name
              (with-syntax ([syn ps]
                            [val pv])
                #`(define-for-syntax (#,parser-name stx)
                    (syntax-parse stx [syn #`val] [else #f]))))))
    (define node-parsers (map node-parser gnodes))
    (let ([fid (get-fid gids)])
      (with-syntax ([(node-parser-ids ...) (map car node-parsers)]
                    [parser-name (parser-id fid)])
        (cons #`parser-name
              (cons #`(define-for-syntax (parser-name stx) (or (node-parser-ids stx) ...))
                    (map cdr node-parsers))))))
  (define group-parsers (map group-parser groups))
  (define all-parsers
    (with-syntax ([parser-name (parser-id oid)]
                  [(group-parser-ids ...) (map car group-parsers)])
      #`(begin (define-for-syntax (parser-name stx) (or (group-parser-ids stx) ...))
               #,@(append-map cdr group-parsers))))
  (printf "parsers: \n") (pretty-print (syntax->datum all-parsers)) (newline)
  all-parsers)
