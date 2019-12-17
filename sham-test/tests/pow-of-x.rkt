#lang racket
(require sham)

(current-sham-module (create-empty-sham-module "pow-module"))

(define (pow-of-x n)
  (define fname (gensym 'pow))
  (values
   fname
   (sham-function
    (,fname (x : i64) : i64)
    (return
     (for/fold ([ast (ui64 1)])
               ((i (in-range n)))
       (mul ast x))))))

(define-values (name f) (pow-of-x 9))
(add-to-sham-module! (current-sham-module) f)

(parameterize ([compile-options (list 'pretty 'dump)])
  (compile-sham-module! (current-sham-module) #:opt-level 3 ))

(sham-app f 10)
