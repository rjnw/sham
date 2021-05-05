#lang racket

(require
 (for-syntax "syntax/spec.rkt"
             "syntax/format.rkt"
             "syntax/private/syntax-class.rkt"
             "syntax/private/utils.rkt"
             racket/match
             racket/list
             racket/syntax
             syntax/strip-context
             syntax/parse))

(provide define-aliases)

(require (for-syntax racket/pretty))

(define-for-syntax sep- #'-)
(define-syntax (group-node (gsep sep-) (nsep sep-)) (ast:format #f sep- 1 gsep nsep))
(define-syntax (minimal (nsep sep-)) (ast:format #f sep- #f sep- nsep))
(define-syntax (define-aliases stx)
  (syntax-parse stx
    [(_ tid:id
        [from:id (~optional (~seq (~datum ->) to:id))] ...
        ki:keyword-info)
     (define ainfo (attribute ki.spec))
     (define aformat (ast-format (or (info-value 'format ainfo) ((syntax-local-value #'group-node)))))
     (define-values (spec _) (syntax-local-value/immediate #`tid))
     (define group-rename-map
       (for/list ([f (syntax-e #`(from ...))]
                  [t (syntax-e #`((~? to from) ...))])
         (cons f t)))
     (define (syn=? s1 s2) (equal? (->symbol s1) (->symbol s2)))
     (define (rename-group orig)
       (assoc-default orig group-rename-map orig syn=?))
     (define (rename-trans pair)
       (match-define (cons from to) pair)
       #`(define-syntax #,from (make-rename-transformer #'#,to)))
     (define (pre s new old)
       (cons (format-id new "~a~a" s new) (format-id old "~a~a" s old)))
     (define (post s new old)
       (cons (format-id new "~a~a" new s) (format-id old "~a~a" old s)))
     (define (generate-group-renames f t)
       (match-define (ast:group gids ginfo gparent gargs gnodes) (find-group-spec f spec))
       (define gprs (map rename-group (group-parents f spec)))
       (define old-fid (get-fid gids))
       (define new-fid (rename-group f))
       (define (generate-node-renames ns)
         (match-define (cons _ (ast:node nids ninfo nargs npat)) ns)
         (define new-fid (replace-context t (ast-format-id #'tid (reverse gprs) (get-oid nids) aformat)))
         (define old-fid (get-fid nids))
         (define args (for/list ([a nargs])
                        (define arg-id (get-fid (get-ast-id a)))
                        (post (format-id #f "-~a" arg-id) new-fid old-fid)))
         `((,new-fid . ,old-fid)
           ,(pre "make-" new-fid old-fid)
           ,(post "?" new-fid old-fid)
           ,@args))
       (define node-renames (append-map generate-node-renames gnodes))
       (define group-renames
         (if (and gparent (syn=? (rename-group gparent) new-fid))
             '()
             (cons (post "?" new-fid old-fid)
                   (for/list ([a gargs])
                     (define arg-id (get-fid (get-ast-id a)))
                     (post (format-id #f "-~a" arg-id) new-fid old-fid)))))
       (map rename-trans (append group-renames node-renames)))
     (define all-renames
       (flatten
        (for/list ([r group-rename-map])
          (match-define (cons f t) r)
          (generate-group-renames f t))))
     #`(begin #,@(flatten all-renames))]))
