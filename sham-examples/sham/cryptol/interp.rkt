#lang racket

(require "ast.rkt"
         sham/sam/compiler)

(define (add-args keys vals hsh)
  (foldl (λ (k v h) (hash-set h k v)) hsh keys vals)
  ;; (hash-union hsh
  ;;             (make-hash (map cons keys vals)))
  )

(define-compiler (cry-interp [ctxt (make-immutable-hash)])
  ;; #:extra-operator with
  (cry -> rkt)
  (itop (top -> any)
        [(mod id (^ decls) ...)
         `(mod ,id ,decls)])
  (idecl (decl -> any)
         [(def name typ (^ bind) ...)
          `(idef ,name ,typ ,bind)]
         [(bind name (vars ...) body)
          ;; [(vars ...) body]
          (cons name
                (lambda iargs
                  (with
                   ([ctxt (add-args vars iargs ctxt)])
                   (cry-interp body))))]
         )
  ;; (iexpr (expr -> any)
  ;;        [(annot e t) (cry-interp e)]
  ;;        [(app (^ o) (^ a) ...) (apply-cry o a)]
  ;;        [(if (chks thns) ... els)
  ;;         (let rec [(chk chks)
  ;;                   (thn thns)]
  ;;           (match* (chk thn)
  ;;             [('() '()) (cry-interp thn)]
  ;;             [((cons c chk) (cons t thn))
  ;;              (if (cry-interp c) (cry-interp t) (rec chk thn))]))]
  ;;        [(var n) (lookup-ctxt n)]
  ;;        [(where body (^ ibinds) ...)
  ;;         (with ([ctxt (extend-bindings ibinds ctxt)])
  ;;               (cry-interp body))])
  ;; (ibit (bit -> any)
  ;;       [true #t]
  ;;       [false #f])
  ;; (inum (numeric -> any)
  ;;       [zero 0]
  ;;       [int int])
  ;; (iseq (sequence -> any)
  ;;       [(basic (^ vs) ...)
  ;;        (apply vector vs)])
  )
