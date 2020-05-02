#lang racket

(require sham/private/env)
(require syntax/parse/define
         (for-syntax syntax/parse racket/syntax
                     syntax/parse/define))

(provide (all-defined-out))

(define (general-ref-key md key)
  (cond [(hash? md) (hash-ref md key #f)]
        [(assoc-env? md) (assoc-env-ref md key)]
        [else #f]))

(define (general-set-key md key value)
  (cond
    [(and (immutable? md) (hash? md)) (hash-set md key value)]
    [(hash? md) (begin (hash-set! md key value) md)]
    [(assoc-env? md) (assoc-env-extend md key value)]
    [(false? md) (hash-set! (make-hash) key value)]
    [else (error 'sham:md "unknown metadata value:~a" md)]))

(define (genral-lambda key)
  (case-lambda
    [(md) (general-ref-key md key)]
    [(md value) (general-set-key md key value)]))

(define (md-ref md key (fail #f)) (if md (hash-ref md key fail) fail))
(define (md-ref! md key fail) (hash-ref! md key fail) md)
(define (md-set! md key val) (hash-set! md key val) md)

(define-syntax (define-key stx)
  (syntax-parse stx
    [(_ key:id)
     #:with ref-name (format-id #'key "ref-~a" #'key)
     #:with set-name (format-id #'key "set-~a!" #'key)
     #:with key-value (format-id #'key "#%md-key:~a" #'key)
     #:with when-name (format-id #'key "when-~a" #'key)
     #`(begin (define key `key-value)
              (define (ref-name md) (md-ref md key))
              (define (set-name md value) (md-set! md key value))
              (define-simple-macro (when-name md v todo (... ...))
                (let ([v (ref-name md)])
                  (when v todo (... ...)))))]))

(define-key metadata-kind)
(define (metadata? v) (and (hash? v) (hash-has-key? v metadata-kind)))

(define-simple-macro (define-md name [keys ...])
  #:with md-name (format-id #'name "~a-md" #'name)
  #:with md-kind-key (format-id #'name "#%md-kind:~a" #'name)
  #:with empty-name (format-id #'md-name "empty-~a" #'md-name)
  #:with check-name (format-id #'md-name "~a?" #'md-name)
  #:with (key-names ...) (map (λ (k) (format-id k "~a-~a" #'md-name k))
                              (syntax->list #`(keys ...)))
  (begin
    (define (empty-name (assocs null)) (make-hash (list (cons (list metadata-kind `md-kind-key) assocs))))
    (define (check-name v) (and (metadata? v) (equal? (md-ref v metadata-kind) `md-kind-key)))
    (define-key key-names) ...))
