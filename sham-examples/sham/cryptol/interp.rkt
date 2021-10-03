#lang racket

(require syntax/parse
         racket/syntax)

(require "ast.rkt"
         sham/sam/transform
         sham/sam/rkt)

(define (get-actual-function-args ityp) '())
(define (get-poly-fuction-args ityp) '())

;; (define-transform (cry-type)
;;   (cry-ast -> rkt-value)
;;   (itype (type -> any)
;;          [bit 'bit]
;;          [integer 'int]
;;          [(sequence (^ d) (^ t)) (iseq d t)]
;;          [(tuple (^ t) ...) (ityp t)]
;;          [(var (^ name)) (itvar name)]
;;          [(poly ((^ args) ...) t)
;;           (ipoly args (^ t))]
;;          [(constraint (cs ...) (^ t)) (ics cs t)]
;;          [(func (^ f) (^ t)) (ifunc f t)])

;;   (idim (dim -> any)
;;         [(and (? integer?) i) i]
;;         [(app (^ rator) (^ rands) ...)
;;          (itapp rator rands)]
;;         [(var (^ name))
;;          (itvar name)]))

;; (define (cry-type-name stxid) (format-id stxid "type:~a" stxid))

(define-transform (cry-interp)
  (cry-ast -> rkt-syntax)
  (itop (top -> stx)
        [(mod id (^ ds) ...)
         (module id ds ...)])
  (idef (def -> stx)
        [(gen (^ ds) ...) (begin ds ...)]
        [(val (^ name) (^ body)) (define (name) body)]
        [(type (^ name) t) (void)]
        [(typeof (^ name) t) (void)]
        [(test (^ name) (^ e1) (^ e2)) (define (name) (equal? e1 e2))]
        [(bind ((^ p) ...) (^ res))
         (lambda (iargs . args)
           (match args
             [(list p ...) res]))])

  (ipat (pat -> stx)
        [(var name) name]
        [(tuple (^ ps) ...) #(ps ...)]
        [(sequence (^ ps) ...) (icry-seq ps ...)])

  (iexpr (expr -> any)
         [(app (^ o) ((^ i) ...) (^ a) ...) (o (list i ...) a ...)]
         [(cond ((^ chks) (^ thns)) ... (^ els))
          (cond [chks thns] ... [else els])]
         [(var (^ n)) (n)]
         [(tvar (^ n)) (cry-lookup-type-var n)]
         [(annot t (^ e)) e]
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
        [(basic (^ vs) ...) (stream vs ...)]
        [(enum (^ from) (^ step) (^ to))
         (icry-from-enum from step to)]
        [(comp (^ body) [((^ var) (^ val))] ...)
         (for/stream ([var val] ...) body)]))
