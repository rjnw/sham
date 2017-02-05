#lang racket

(provide statement-expander)

(define (statement-expander ast)
  (define se statement-expander)
  (match ast
    [`(for1 ((,i ,iv ,n : ,typ) ,t) ,b ...)
     (statement-expander `(for ((,i ,iv ,n : ,typ) ,t) ,@b))]
    [`(for (((,indexs ,init-values ,next-vals : ,types) ...) ,tst) ,bodys ...)
     `(let (,@(map (λ (i t) `(,i : ,t)) indexs types))
        (block
         ,@(map (λ (i v) `(set! ,i ,v)) indexs init-values)
         (while ,tst
           (block
            ,@bodys
            ,@(map (λ (i v) `(set! ,i ,v)) indexs next-vals)))))]
    [`(fold-loop-set! ,set-to
                      (,i ,init-i ,end-exp : ,type-i)
                      (,acc ,init-acc : ,type-acc)
                      ,bodys ...)
     `(let ((,i : ,type-i)
            (,acc : ,type-acc))
        (block
         (set! ,i ,init-i)
         (set! ,acc ,init-acc)
         (while ,end-exp
           ,(if (eq? (length bodys) 1)
                bodys
                `(block ,@bodys)))
         (set! ,set-to ,acc)))]
    [else ast]))

(module+ test
  (pretty-print
   (statement-expander `(for (((i 0 (+ i 1) : int))
                              (< i 10))
                          (print i)))))
