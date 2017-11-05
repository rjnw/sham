#lang racket

(require racket/splicing)
(provide (all-defined-out))

(define (vlb v t)
  `(#%value ,v ,t))
(define (vi v) (vlb v 'int))
(define (vui v) (vlb v 'uint))
(define (vsb v) (vlb v 'sbyte))
(define (vub v) (vlb v 'ubyte))
(define (vsh v) (vlb v 'short))
(define (vus v) (vlb v 'ushort))
(define (vl v) (vlb v 'long))
(define (vul v) (vlb v 'ulong))
(define (vf32 v) (vlb v 'float32))
(define (vf64 v) (vlb v 'float64))

(define ((ap rator) . rands)
  `(#%app ,rator ,@rands))

(splicing-let ([gensym-hash (make-hash)])
  (define (gensym^ sym [sep ""])
    (define n (add1 (hash-ref gensym-hash sym 0)))
    (hash-set! gensym-hash sym n)
    (string->symbol (string-append (symbol->string sym)
                                   sep
                                   (number->string n)))))

(require (for-syntax racket/syntax))

(define-syntax (register-ops stx)
  (syntax-case stx ()
    [(_ ops)
     #`(begin
         #,@(for/list ([op (syntax->list #'ops)])
              (let* ([op-str (symbol->string (syntax->datum op))])

                (with-syntax ([opd (format-id op "appb-~a" op-str)]
                              [op-rator (format-id op "jit-~a" op-str)])
                  #'(define opd (ap 'op-rator))))))]))

(define (for-loopb var-sym var-type start-exp check-bexp inc-bexp body-bstmt)
  `(define-variable (,var-sym : ,var-type)
     (block
      (assign ,var-sym ,start-exp)
      (while ,(check-bexp var-sym)
        ,(combine-blocks
          `(block)
          ,(body-bstmt var-sym)
          (assign var-sym ,(inc-bexp var-sym)))))))

(define ((add1b [t 'int]) var-sym)
  `(appb-add ,var-sym (vl 1 t)))


;;binary jit ops
(register-ops (null? add add-ovf sub sub-ovf mul mul-ovf div rem rem-ieee and or)
         xor shl shr ushr sshr eq? ne? lt? le? gt? ge? cmpl cmpg)

;;unary jit ops
(register-ops (neg to-bool to-not-bool acos asin atan ceil cos cosh exp floor)
       log log10 rint round sin sinh sqrt tan tanh trunc is-nan
       is-inf is-finite abs sign)

(module+ test
  (pretty-display (for-loopb 'i 'int 0
                             (curryr appb-lt? (vi 10))
                             ;; (lambda (i) ((ap 'jit-lt?) i (vi 10)))
                             (curryr appb-add (vi 1))
                            ;; (lambda (i) ((ap 'jit-add) i (vi 1)))
                            (lambda (i) `(test-exp)))))

;;misc utils
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1))
         (symbol->string s2)))

(define (clean-block stmt)
  (match stmt
    [`(block ,sts ...)
     (define ne-sts (filter (compose not null?) sts))
     (if (eq? 1 (length ne-sts))
         (car ne-sts)
         `(block ,@ne-sts))]
    [else stmt]))

(define (combine-blocks stmt)
  (match stmt
      [`(block (block ,stmts ...) ,estms ..)
       (combine-blocks `(block ,@stmts ,@estms))]
      [`(block ,estms1 ... (block ,stmts ...) ,estms2 ...)
       (combine-blocks `(block ,@estms1 ,@stmts ,@estms2))]
      [else stmt]))
