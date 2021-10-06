#lang racket

(require syntax/parse
         racket/syntax)

(require "../ast.rkt"
         sham/sam/transform
         sham/sam/rkt)

(provide cry-ast-to-rkt-stx)

(define-transform (cry-ast-to-rkt-stx)
  (cry-ast -> rkt-syntax)
  (itop (top -> stx)
        [(mod (^ name) (^ ds) ...)
         (module name ds ...)])
  (idef (def -> stx)
        [(gen (^ ds) ...) (begin ds ...)]
        [(val (^ name) (^ body)) (define (name) body)]
        [(type (^ name) t) (void)]
        [(typeof (^ name) t) (void)]
        [(test (^ name) (^ e1) (^ e2)) (define (name) (equal? e1 e2))])

  (ipat (pat -> stx)
        [(var (^ name)) name]
        [(tuple (^ ps) ...) #(ps ...)]
        [(sequence (^ ps) ...) (icry-seq ps ...)])

  (ibit (bit -> stx)
        [true #t]
        [false #f])

  (iseq (sequence -> stx)
        [(basic (^ vs) ...) (stream vs ...)]
        [(enum (^ from) (^ step) (^ to))
         (icry-from-enum from step to)]
        [(str s) s]
        [(comp (^ body) [((^ var) (^ val)) ...] ...)
         (for/stream ([var val] ...) ... body)])

  (iexpr (expr -> stx)
         [(bind () (^ res)) res]
         [(bind ((^ p) ...) (^ res)) (match-lambda** [(iargs p ...) res])]
         [(app (^ o) ((^ i) ...) (^ a) ...) (o (list i ...) a ...)]
         [(cond ((^ chks) (^ thns)) ... (^ els))
          (cond [chks thns] ... [else els])]
         [(var (^ n)) (n)]
         [(tvar (^ n)) (cry-lookup-type-var n)]
         [(annot (^ e) t) e]
         [(where (^ body) (^ ds) ...)
          (let () ds ... body)]
         [(error msg) (error msg)]
         [(lit i) (icry-from-integer i)]
         [(char c) (icry-from-char c)]
         [(zero) (icry-from-integer 0)]) )
