#lang racket
(require (for-syntax syntax/parse))
(require "../ast.rkt"
         sham/sam/transform
         sham/sam/rkt
         sham/sam/runtime/identifier)

(provide (all-defined-out))

(define (remove-gen-defs ds)
  (flatten
   (let loop ([defs ds])
     (cond [(list? defs) (map loop defs)]
           [(def-gen? defs) (loop (def-gen-ds defs))]
           [else defs]))))

(define (split-defs defs)
  (define val-defs (filter def-val? defs))
  (define type-defs (filter def-type? defs))
  (define typeof-defs (filter def-typeof? defs))
  (define test-defs (filter def-test? defs))
  (values val-defs type-defs typeof-defs test-defs))

(define (find-typeof name tdefs)
  (define (is-type? tdef)
    (match-define (def-typeof tnames ... t) tdef)
    (ormap (λ (tname) (free-identifier=? (ast-id-stxid name) (ast-id-stxid tname))) tnames))
  (findf is-type? tdefs))

(define (combine-val&typeof vdefs tdefs)
  (for/fold ([res '()])
            [(d vdefs)]
    (match-define (def-val name e) d)
    (cons (cons d (find-typeof name tdefs)) res)))


(define debug? (make-parameter #t))

(define-syntax (debug stx)
  (syntax-case stx ()
    [(_ es ...)
     #`(when (debug?) es ...)]))

(define (debug-print v) (debug (printf "debug: ") (pretty-print v) (newline)) v)

(define-syntax (TODO stx)
  (syntax-parse stx
    [(_ s:string args:expr ...)
     #`(error 'sham/cry/todo s args ...)]
    [else #`(error 'sham/cry/todo #,(format "~a" stx))]))

(define-transform (pretty-cry-ast)
  (cry-ast -> rkt-value)
  (cdef (def -> any)
        [(gen (^ ds) ...) ds]
        [(val n (^ v)) `(,n = ,v)]
        [(typeof ns ... (^ v)) `(,@ns : ,v)]
        [(type n (^ t)) `(type ,n : ,t)]
        [(test n (^ v1) (^ v2)) `(test ,n : ,v1 = ,v2)])
  (cpat (pat -> any)
        [(var n) n]
        [(tuple (^ ps) ...) `(tuple ,@ps)]
        [(sequence (^ ps) ...) (apply vector ps)])
  (cexpr (expr -> any)
         [(bind ((^ ps) ...) (^ b)) `(,@ps = ,b)]
         [(app (^ o) ((^ ia) ...) (^ a) ...) `(,o {,@ia} ,@a)]
         [(cond ((^ c) (^ t)) ... (^ e)) `(cond ,@(map cons c t) ,e)]
         [(var n) n]
         [(tvar n) `',n]
         [(annot (^ e) (^ t)) `(,e : ,t)]
         [(where (^ b) (^ ds) ...) `(where ,b ,ds)]
         [(error msg) `(error ,msg)]
         [(lit i) i]
         [(char c) c]
         [(tuple (^ vs) ...) `($ ,@vs)])
  (cseq (sequence -> any)
        [(basic (^ vs) ...) (apply vector vs)]
        [(enum (^ f) (^ s) (^ t)) `(,f .. ,s ,t)]
        [(str s) `(str ,s)]
        [(comp (^ body) ((v (^ l))) ...)
         `[,body \| ,@(map cons v l)]])
  (ctyp (type -> any)
        [bit 'bit]
        [integer 'int]
        [(sequence (^ d) (^ t)) (vector d t)]
        [(tuple (^ ts) ...) `(tuple ,@ts)]
        [(var n) n]
        [(poly (vars ...) (^ t)) `(,vars : ,t)]
        [(constraint (cs ...) (^ t)) `(,@(map syntax->datum cs) => ,t)]
        [(func (^ f) (^ t)) `(,f -> ,t)])
  (cdim (dim -> any)
        [(int v) v]
        [(app op (^ args) ...) `(,(syntax->datum op) ,@args)]
        [(var n) `(dim ,n)]))

(define (pretty-cry v) (if (struct-cry-ast? v) (pretty-cry-ast v) v))
