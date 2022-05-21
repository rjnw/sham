#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(define (needs-ptr-type? t)
  (match type
    [(type-seq d (type-bit)) #f]
    [(type-seq d t) #t]
    [(type-tup ts ...) #t]
    [else #f]))
(struct ref [v] #:transparent)
;; allocation
(define (sham-alloca stype) #`(alloca #,stype))
(define (sham-arr-alloca stype len) #`(arr-alloca #,stype #,len))
;; basic funs
(define (sham-test name body) #`(make-def-function #,name test-function-type #,body))
(define (sham-fun name type body) #`(make-def-function #,name #,type #,body))
(define (sham-test-final) #`(stmt-return-void))
(define (sham-fun-final) #`(stmt-return (ui64 0)))
(define sham-bit-type #`bit-type)
(define sham-int-type #`i64)
(define sham-idx-type #`i64)
(define (sham-tup-type ts) #`(tup-type #,@ts))
(define (sham-seq-type d ts) #`(seq-type #,ts))
(define (sham-ptr-type t) #`(ptr-type #,t))
(define (sham-fun-type args to) #`(basic-function-type (list #,@args #,to)))

(define (sham-void) #`(stmt-void))
(define (sham-evoid) #`(expr-void))
(define (sham-var-ref n) (ref #`(expr-ref #,n)))
(define (sham-store to val)
  (if (ref? to)
      #`(store-val! #,val #,(ref-v to))
      (error 'sham/cry "store needs ref: ~a" to)))

(define (sham-dim-val v) (sham-int-literal v (type-idx)))
(define (sham-int-literal i type)
  (match type
    [(type-seq 64 (type-bit)) #`(ui64 #,i)]
    [(type-seq 32 (type-bit)) #`(ui32 #,i)]
    [(type-seq (and d (? integer?)) (type-bit)) #`(ll-val-ui #,(number->string i) (ll-type-ref #,d))]
    [(type-idx) #`(ui64 #,i)]
    [(type-int) (printf "WARN: using generic integer type: ~a\n" i) #`(ui64 #,i)]
    [else (error 'todo-sham "unknown integer ~a ~a" i type)]))

(define (sham-seq-upto-ref val) #`(seq-upto-ptr #,val))
(define (sham-seq-len-ref val) #`(seq-len-ptr #,val))
(define (sham-seq-len val) #`(op-load (seq-len-ptr #,val)))
(define (sham-seq-arr-ref val) #`(seq-array-ptr-ptr #,val))
(define (sham-seq-idx-ref val idx) #`(seq-index-ptr #,val #,idx))
(define (sham-seq-idx val idx) #`(op-load (seq-index-ptr #,val #,idx)))
(define (sham-tup-idx-ref val idx) #`(tup-index-ptr #,val #,idx))
(define (sham-tup-idx val idx) #`(op-load (tup-index-ptr #,val #,idx)))

(define (sham-val v)
  (cond [(ref? v) #`(op-load #,(ref-v v))]
        [else v]))
(define (sham-farg idx) #`(expr-ref #,idx))
(define (sham-if chk thn els) #`(stmt-if #,chk #,thn #,els))
(define (sham-err s) #`(cprintf (format "err: ~a\n" s)))
(define (sham-stmts ss) #`(stmt-block #,@ss))
;; vars = (list (list name val type))
(define (sham-bind vars body) #`(stmt-let #,vars #,body))

(define (sham-cmp-int v1 v2) #`(op-icmp-eq #,v1 #,v2))
(define (sham-andn cs)
  (match cs
    [(list c1) c1]
    [(cons fst rst) #`(op-and #,fst #,(sham-andn rst))]
    ['() #`primitive-true]))

(define (int-ult-loop body start end idx-sym (step #`(ui64 1)))
  (define idx #`(expr-ref #,idx-sym))
  #`(stmt-let
     ([#,idx-sym #,start i64])
     (stmt-while
      (op-icmp-ult #,idx #,end)
      (stmt-block
       #,body
       (stmt-set! #,idx (op-add #,idx #,step))))))
(define (sham-iterate-for-seq dim idx-sym body)
  (int-ult-loop body #(ui64 0) #(ui64 #,dim) idx-sym))
(define require-syntax
  #`((require sham
              sham/jit
              sham/cryptol/prelude/sham
              (prefix-in ll- sham/llvm/ir))))

(define extra-syntax
  #`((module+ test
       (define built-sham-env (build-sham-env cryptol-module #:debug #t))
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-llvm-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       ;; (sham-env-optimize-llvm! built-sham-env #:opt-level 2)
       (sham-print-llvm-ir built-sham-env)
       (printf "verifying-opt-ir: ~a\n" (sham-verify-llvm-ir built-sham-env))
       (define built-jit-env (initialize-jit built-sham-env))
       (define test-runner (jit-lookup-function built-jit-env 'run-tests))
       (printf "running-cryptol-tests:\n")
       (test-runner))))
