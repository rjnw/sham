#lang racket

(require syntax/parse
         racket/syntax
         (for-template racket))

(require "../ast.rkt"
         sham/sam/transform
         sham/sam/runtime/identifier
         sham/sam/rkt)

(provide cry-ast-to-rkt-stx)

(define (mapping-contains? ids stxid)
  (member stxid ids free-identifier=?))
(define (add-pat-ids ids bind-ast)
  (printf "add-pat-ids: ~a\n" bind-ast)
  (define (in-pat p)
    (match p
      [(cry-ast:pat:var n) (ast-id-stxid n)]
      [(cry-ast:pat:tuple p ...) (append-map in-pat p)]
      [(cry-ast:pat:sequence p ...) (append-map in-pat p)]))
  (append
   (match bind-ast
     [(cry-ast:expr:bind (p ...) b) (in-pat p)])
   ids))

(define-transform (cry-ast-to-rkt-stx (pat-ids '()))
   (cry-ast -> rkt-syntax)
   (itop (top -> stx)
         [(mod (^ name) (^ ds) ...) (module name ds ...)])
   (idef (def -> stx)
         [(gen (^ ds) ...) (begin ds ...)]
         [(val (^ name) (^ body)) (define (name) body)]
         [(type (^ name) t) (void)]
         [(typeof (^ name) t) (void)]
         [(test (^ name) (^ e1) (^ e2)) (define (name) (equal? e1 e2))])

   (ipat (pat -> stx)
         [(var (^ name)) name]
         [(tuple (^ ps) ...) (vector ps ...)]
         [(sequence (^ ps) ...) (icry-seq ps ...)])

   (iseq (sequence -> stx)
         [(basic (^ vs) ...) (stream vs ...)]
         [(enum (^ from) (^ step) (^ to))
          (icry-from-enum from step to)]
         [(str s) s]
         [(comp (^ body) [((^ var) (^ val)) ...] ...)
          (for/stream ([var val] ...) ... body)])

   (iexpr (expr -> stx)
          [(bind () (^ res)) res]
          [(bind ((^ p) ...) (^ res #:pat-ids (add-pat-ids pat-ids this-ast))) (match-lambda** [(iargs p ...) res])]
          [(app (^ o) ((^ i) ...) (^ a) ...) (o (list i ...) a ...)]
          [(cond ((^ chks) (^ thns)) ... (^ els))
           (cond [chks thns] ... [else els])]
          [(var (^ name)) #,(if (mapping-contains? pat-ids #`name) #`name #`(name))]
          [(tvar (^ n)) (cry-lookup-type-var n)]
          [(annot (^ e) t) e]
          [(where (^ body)) body]
          [(where (^ body) (^ ds) ...)
           (let () ds ... body)]
          [(error msg) (error msg)]
          [(lit i) (icry-from-integer i)]
          [(char c) (icry-from-char c)]
          [(zero) (icry-from-integer 0)]) )
