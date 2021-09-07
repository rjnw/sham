#lang racket

(require syntax/parse)

(require "ast.rkt"
         sham/sam/compiler)

(define-compiler (cry-interp)
  (cry -> rkt-syntax)
  (itop (top -> stx)
        [(mod id (^ decls) ...)
         (define (id) decls)])
  (idecl (decl -> stx)
         [(def name typ (^ binds) ...)
          (define name (case-lambda binds ...))]
         [(bind ((^ vars) ...) (^ body))
          [(vars ...) body]]
         [(value (^ e)) e])
  (iexpr (expr -> any)
         [(app (^ o) (^ a) ...) (o a ...)]
         [(ifcond ((^ chks) (^ thns)) ... (^ els))
          (cond [chks thns] ... [else els])]
         [(var (^ n)) (n)]
         [(where body ((^ bname) (^ ibinds)) ...)
          (letrec ([bname (case-lambda ibinds)] ...))])
  (ibit (bit -> stx)
        [true #t]
        [false #f])
  (inum (numeric -> stx)
        [zero 0]
        [(int (^ i)) i])
  (iseq (sequence -> stx)
        [(basic (^ vs) ...) (stream vs ...)]
        [(comp (^ body) [((^ var) (^ val)) ...] ...)
         (for/stream (~@ [(var val) ...] #:when #t) ...
           body)]))
