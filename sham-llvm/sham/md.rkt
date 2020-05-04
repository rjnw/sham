#lang racket

(require sham/private/md
         sham/private/keyword)

(require racket/hash)

(provide (all-defined-out)
         metadata?
         general-ref-key
         general-set-key
         md-ref
         md-ref!
         md-set!)

(define-md module
  [llvm-after-passes
   llvm-before-passes
   jit-external-mappings])

(define-md function
  [llvm-general-attributes
   llvm-return-attributes
   llvm-argument-attributes
   llvm-optimization-pass
   llvm-calling-convention
   jit-rkt-type])

(define-md instruction [llvm-calling-convention])

(define-md struct [llvm-packed])

(define-md type [special-rkt])

(define-md sham-env [rkt-types])


(define-general-keyword-procedure
  (sham-function-metadata ka . rest-args)
  (define source-mds
    (if (andmap function-md? rest-args)
        rest-args
        (error 'sham:md "unknown metadata give to sham-function-metadata ~a" rest-args)))
  (define md (empty-function-md))
  (apply (curry (curry hash-union! md) #:combine/key (λ (k a b) b)) source-mds)
  (define attribute-map
    (list
     (cons '(#:calling-convention #:convention #:call-conv)
           set-function-md-llvm-calling-convention!)
     (cons '(#:general-attributes #:attributes)
           set-function-md-llvm-general-attributes!)
     (cons '(#:return-attribute)
           set-function-md-llvm-return-attributes!)
     (cons '(#:argument-attributes)
           set-function-md-llvm-argument-attributes!)
     (cons '(#:opt-pass #:optimization-pass #:llvm-pass #:llvm-opt-pass)
           set-function-md-llvm-optimization-pass!)
     (cons '(#:rkt-type #:racket-type)
           set-function-md-jit-rkt-type!)))
  (for ([am attribute-map])
    (match-define (cons keys setf!) am)
    (cond
      [(lookup-keyword ka keys (const #f))
       =>
       (curry setf! md)]))
  md)

(define-general-keyword-procedure
  (sham-module-metadata ka . rest-args)
  (define source-mds
    (if (andmap module-md? rest-args)
        rest-args
        (error 'sham:md "unknown metadata give to sham-module-metadata ~a" rest-args)))
  (define md (empty-module-md))
  (apply (curry hash-union! md) source-mds)
  (define attribute-map
    (list
     (cons '(#:after-pass #:late-pass)
           set-module-md-llvm-after-passes!)
     (cons '(#:early-pass #:before-pass)
           set-module-md-llvm-before-passes!)
     (cons '(#:external-mappings #:externals #:jit-externals)
           set-module-md-jit-external-mappings!)))
  (for ([am attribute-map])
    (match-define (cons keys setf!) am)
    (cond
      [(lookup-keyword ka keys (const #f))
       =>
       (curry setf! md)]))
  md)
