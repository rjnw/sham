#lang racket

(require racket/generic
         "spec.rkt"
         (for-template (prefix-in rt: "runtime.rkt")
                       racket))

(provide (all-defined-out))

(define-generics ast-constructor
  (construct-top-struct ast-constructor ast-spec)
  (construct-group-struct ast-constructor ast-spec group-spec)
  (construct-node-struct ast-constructor ast-spec group-spec node-spec))

(define-generics ast-construct
  (->syntax ast-construct)
  #:defaults
  ([syntax?
    (define (->syntax c) c)]
   [(list/c (or/c ast-construct? false/c))
    (define (->syntax c)
      (map ->syntax (filter (compose not false?) c)))]))

(define-generics ast-builder
  (build-top-struct ast-builder struct-syntax ast-spec)
  (build-group-struct ast-builder ast-spec group-syntax group-spec)
  (build-group-extra ast-builder ast-spec group-spec)
  (build-node-struct ast-builder node-syntax ast-spec group-spec node-spec)
  (build-node-extra ast-builder ast-spec group-spec node-spec)
  #:defaults
  ([any/c
    (define (build-top-struct ab syn as) syn)
    (define (build-group-struct ab syn as gs) syn)
    (define (build-group-extra ab as gs) #f)
    (define (build-node-struct ab syn as gs ns) syn)
    (define (build-node-extra ab as gs ns) #f)]))

;; wrapper around make-struct-type arguments
(struct ast:struct:rkt [name super-type args auto-args auto-val props insp proc-spec immutables guard constructor-name]
  #:methods gen:ast-construct
  [(define (->syntax asr)
     (match-define (ast:struct:rkt name parent args auto-args auto-val props insp proc-spec immutables guard constructor-name) asr)
     (with-syntax
       ([(a ...) args]
        [(aa ...) auto-args]
        [n name])
       #`(struct n #,@(if parent (list parent) (list))
           [a ... (aa #:auto) ...]
           #,@(if constructor-name (list #`#:reflection-name #``#,constructor-name) (list)))))])

(define (ast-struct-rkt name super-type (args '()) (auto-args '())
                        (auto-val #f) (props '()) (insp #f) (proc-spec #f) (immutables #f)
                        (guard #f) #:name (constructor-name #f))
  (ast:struct:rkt name super-type args auto-args auto-val props insp proc-spec immutables guard constructor-name))

(struct rkt-ast-constructor [spec]
  #:methods gen:ast-constructor
  [(define (construct-top-struct arc as)
     (match-define (ast id sid groups info) as)
     (ast-struct-rkt sid #`rt:ast #:name id))
   (define (construct-group-struct arc as gs)
     (match-define (ast top-id syn-id groups top-info) as)
     (match-define (ast:group gid gsyn-id parent gargs-assoc nodes info) gs)
     (ast-struct-rkt gsyn-id (or parent syn-id) (map car gargs-assoc)))
   (define (construct-node-struct arc as gs ns)
     (match-define (ast:group gid gsyn-id parent gargs-assoc nodes ginfo) gs)
     (match-define (ast:node nid nsyn-id nargs-assoc pat ninfo) ns)
     (ast-struct-rkt nsyn-id gsyn-id (map car nargs-assoc)))])
