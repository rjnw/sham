#lang racket

(require rcf/ast
         sham
         sham/ast-utils
         sham/jit-utils
         (for-syntax syntax/parse racket/pretty racket/set))

(define fsa-module
  (create-empty-sham-module "module"))
(current-sham-module fsa-module)

(define-syntax (define-fsa stx)
  (syntax-parse stx
    [(_ name start (end ...) [state ([evt next-state] ...)] ...)
     (define state-id-map
       (make-hash (for/list ([s (syntax->list #`(state ...))]
                             [i (in-range (length (syntax->list #`(state ...))))])
                    (cons (syntax->datum s) (add1 i)))))
     (define end-states (list->set (syntax->datum #`(end ...))))
     (pretty-display state-id-map)
     (define funcs
       #`(begin
           (define-sham-function (state (inp : i64*) (pos : i64) (len : i64) : i64)
             (if^ (icmp-ule pos len)
                  (switch^ (load (gep^ inp pos))
                           [(ui64 evt) (return (next-state inp (add pos (ui64 1)) len))] ...
                           (return (ui64 0)))
                  (return (ui64 1)))) ...))
     (pretty-display (syntax->datum funcs))
     (define fs
       (for/list  ([s (syntax->list #`(state ...))]
                   [sevt (syntax->list #`((evt ...) ...))]
                   [snxs (syntax->list #`((next-state ...) ...))])
         (define f
           #`(define-sham-function (#,s (inp : i64*) (pos : i64) (len : i64) : i64)
               (if^ (icmp-ult pos len)
                    (switch^ (load (gep^ inp pos))
                             #,@(map (λ (s ns) #`[(ui64 #,s)
                                                  (return (#,ns inp (add pos (ui64 1))))])
                                     (syntax->list sevt) (syntax->list snxs))
                             (return (ui64 0)))
                    #,(if (set-member? end-states (syntax->datum s))
                          #`(return (ui64 1))
                          #`(return (ui64 0))))))
         f))
     (pretty-display (map syntax->datum fs))
     #`(begin #,@fs)]))

(define-fsa M
  s1 (s1)
  [s1 ([0 s2]
       [2 s1])]
  [s2 ([0 s1]
       [2 s2]
       [4 s2])])

(parameterize ([compile-options (list 'pretty 'dump 'verify)])
  (compile-sham-module!
   fsa-module
   #:opt-level 0))

(sham-app s1 #f 0 0)
(require ffi/unsafe)
(sham-app s1 (list->cblock '(2 0 2 2 0) _uint64) 0 5)

;; (define (s1 e)
;;   (match e
;;     [0 s2]
;;     [2 s1]))
;; (define (s2 e)
;;   (match e
;;     [0 s1]
;;     [2 s2]
;;     [4 s2]))
;; (define M (cons s1 (list s1)))
;; (define (machine-accepts? M ls)
;;   (define final
;;     (for/fold ([current (car M)])
;;               ([l ls])
;;       (current l)))
;;   (not (false? (member final (cdr M)))))
;; (machine-accepts? M (list 2 0 4 0 2))
