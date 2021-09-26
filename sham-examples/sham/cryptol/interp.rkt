#lang racket

(require syntax/parse
         racket/syntax)

(require "ast.rkt"
         sham/sam/compiler)
(define (get-actual-function-args ityp)
  '())
(define (get-poly-fuction-args ityp)
  '())

(define-transform (cry-type)
  (cry -> rkt-value)
  (itype (type -> any)
         [bit 'bit]
         [integer 'int]
         [(sequence (^ d) (^ t)) (iseq d t)]
         [(tuple (^ t) ...) (ityp t)]
         [(var (^ name)) (itvar name)]
         [(poly ((^ args) ...) t)
          (ipoly args (^ t))]
         [(constraint (cs ...) (^ t)) (ics cs t)]
         [(func (^ f) (^ t)) (ifunc f t)])

  (idim (dim -> any)
        [(and (? integer?) i) i]
        [(app (^ rator) (^ rands) ...)
         (itapp rator rands)]
        [(var (^ name))
         (itvar name)]))

(define (cry-type-name stxid) (format-id stxid "type:~a" stxid))

(define-transform (cry-interp maybe-type)
  (cry -> rkt-syntax)
  (itop (top -> stx)
        [(mod id (^ decls) ...)
         (module id decls ...)])
  (idef (def -> stx)
        [(val (^ typ) (^ binds) ...)
         #:with fargs (get-actual-function-args typ)
         #:with-stx (args ...) (generate-temporaries (build-list (length fargs) (thunk #`v)))
         #:with-stx (iargs ...) (get-poly-function-args typ)
         (define (name iarg args ...)
           (match-define (list iargs ...) iarg)
           (match* (args ...) (^ binds #:maybe-type typ) ...))]
        [(type (^ name) (^ t))
         #:with-stx tname (cry-type-name #`name)
         (define tname t)]
        [(typeof (^ name) (^ t)) (set-cry-value-type! name t)]
        [(bind ((^ vars) ...) (^ body))
         [(vars ...) body]])

  (ipat (pat -> stx)
        [(var name) name]
        [(tuple (^ ps) ...) #(ps ...)]
        [(sequence (^ ps) ...)
         (icry-seq ps ...)])

  (iexpr (expr -> any)
         [(app (^ o) (^ a #:maybe-type (cry-args-of (cry-typeof (v o)))) ...) (o a ...)]
         [(cond ((^ chks #:maybe-type ibit) (^ thns)) ... (^ els))
          (cond [chks thns] ... [else els])]
         [(var (^ n)) (n)]
         [(tvar (^ n)) (cry-lookup-type-var n)]
         [(annot (^ t) (^ e #:maybe-type t)) e]
         [(where (^ body) (^ ds) ...)
          (let () ds ... body)]
         [(error msg) (error msg)]
         [(lit i) (icry-from-integer i)]
         [(char c) (icry-from-char c)]
         [(zero) (icry-from-integer 0)])
  (ibit (bit -> stx)
        [true #t]
        [false #f])

  (iseq (sequence -> stx)
        [(basic (^ vs #:maybe-type (cry-elem-of maybe-type)) ...) (stream vs ...)]
        [(enum (^ from) (^ step) (^ to))
         (icry-from-enum from step to)]
        [(comp (^ body #:maybe-type (cry-elem-of maybe-type)) [((^ var) (^ val))] ...)
         (for/stream ([var val] ...) body)]))
